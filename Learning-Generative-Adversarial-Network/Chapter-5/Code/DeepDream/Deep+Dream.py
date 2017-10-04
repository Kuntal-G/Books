
# coding: utf-8

# # Create Artistic Hallucination

# In[13]:


import numpy as np
from scipy.misc import imsave
from scipy.optimize import fmin_l_bfgs_b
import time

from keras.preprocessing.image import load_img, img_to_array
from keras.applications import vgg16
from keras.layers import Input
from keras import backend as K

K.set_image_dim_ordering('tf')


# In[ ]:


# base image path and result prefix
### Change the path to your own image
base_image_path = '/Users/kuntalg/Desktop/deepdream-pic/kun.jpg'
result_prefix = '/Users/kuntalg/Desktop/deepdream-pic/kg'

# dimensions of the generated image
img_width, img_height = 1200, 750


# In[15]:


# some settings we found interesting
settings_preset = {
    'dreamy': {
        'features': {
            'block5_conv1': 0.05,
            'block5_conv2': 0.02
        },
        'continuity': 0.1,
        'dream_l2': 0.02,
        'jitter': 0
    }
}


# settings to be used
settings = settings_preset['dreamy']


# In[16]:


""" Utility functions """
# util function to open, resize and format images to appropriate tensors
def preprocess_image(image_path):
    img = load_img(image_path, target_size=(img_height, img_width))
    img = img_to_array(img)
    img = np.expand_dims(img, axis=0)
    img = vgg16.preprocess_input(img)
    return img

# util function to convert a tensors into a valid image
def deprocess_image(x):
    x = x.reshape((img_height, img_width, 3)) # using 'tf' backend

    # remove zero-center by mean pixel
    x[:, :, 0] += 103.939
    x[:, :, 1] += 116.779
    x[:, :, 2] += 123.680

    # convert BGR to RGB
    x = x[:, :, ::-1]
    x = np.clip(x, 0, 255).astype('uint8')

    return x


# In[17]:


# continuity loss: to give the image local coherence and avoid messy blurs
# looks like a variant of the total variation loss; this paper discusses the use
# of a total variation prior in the context of synthesizing images with convnets:
# http://www.robots.ox.ac.uk/~vedaldi/assets/pubs/mahendran15understanding.pdf
def continuity_loss(x):
    assert K.ndim(x) == 4
    a = K.square(x[:, :img_height-1, :img_width-1, :] -
                 x[:, 1:, :img_width-1, :])
    b = K.square(x[:, :img_height-1, :img_width-1, :] -
                 x[:, :img_height-1, 1:, :])

    # (a+b) is the squared spatial gradient, 1.25 is a hyperparameter that should
    # be >1.0 as discussed in the aforementioned paper
    return K.sum(K.pow(a+b, 1.25))

# util function to evaluate loss and gradient
def eval_loss_and_grads(x):
    x = x.reshape((1,) + img_size)
    outs = f_outputs([x])
    loss_value = outs[0]
    if len(outs[1:]) == 1:
        grad_values = outs[1].flatten().astype('float64')
    else:
        grad_values = np.array(outs[1:]).flatten().astype('float64')
    return loss_value, grad_values


# In[18]:


img_size = (img_height, img_width, 3)
# this will contain the generated image
dream = Input(batch_shape=(1,)+img_size)

# load the vgg16 model with pretrained weights
model = vgg16.VGG16(input_tensor=dream, weights='imagenet', include_top=False)
print('Model loaded.')

# get the symbolic output of each "key" layer
layer_dict = dict([(layer.name, layer) for layer in model.layers])


# In[19]:


# define the loss
loss = K.variable(0.)

for layer_name in settings['features']:
    # add the L2 norm of the features of a layer to the loss
    assert layer_name in layer_dict.keys(), 'Layer ' + layer_name + ' not found in model.'
    coeff = settings['features'][layer_name]
    x = layer_dict[layer_name].output
    shape = layer_dict[layer_name].output_shape
    # avoid border artifacts by only involving non-border pixels in the loss
    loss -= coeff * K.sum(K.square(x[:, 2: shape[1]-2, 2: shape[2]-2, :])) / np.prod(shape[1:])

# add continuity loss (to give the image local coherence and avoid messy blurs)
loss += settings['continuity'] * continuity_loss(dream) / np.prod(img_size)

# add image L2 norm to loss (prevents pixels from taking very high values)
loss += settings['dream_l2'] * K.sum(K.square(dream)) / np.prod(img_size)

### modify the loss to achieve new effects

# compute the gradients of the dream wrt the loss
grads = K.gradients(loss, dream)

outputs = [loss]
if isinstance(grads, (list, tuple)):
    outputs += grads
else:
    outputs.append(grads)

f_outputs = K.function([dream], outputs)


# In[20]:


# we need to evaluate our loss and our gradients in one pass, but scipy.optimize
# requires separate functions for loss and gradients, and computing them separately
# would be inefficient. To solve this we create our own Evaluator:
class Evaluator(object):
    def __init__(self):
        self.loss_value = None
        self.grad_values = None

    def loss(self, x):
        assert self.loss_value is None
        loss_value, grad_values = eval_loss_and_grads(x)
        self.loss_value = loss_value
        self.grad_values = grad_values
        return self.loss_value

    def grads(self, x):
        assert self.loss_value is not None
        grad_values = np.copy(self.grad_values)
        self.loss_value = None
        self.grad_values = None
        return grad_values

evaluator = Evaluator()


# In[ ]:


# run L-BFGS optimizer over the pixels of the generated image,
# in order to minimize the loss
x = preprocess_image(base_image_path)

for i in range(20):
    print('Start of iteration', i)
    start_time = time.time()

    # add a random jitter to the initial image; this will be reverted at decoding time
    random_jitter = (settings['jitter']*2) * (np.random.random(img_size)-0.5)
    x += random_jitter

    # run L-BFGS for 7 steps
    x, min_val, info = fmin_l_bfgs_b(evaluator.loss, x.flatten(),
                                     fprime=evaluator.grads, maxfun=7)

    print('Current loss value:', min_val)

    # decode the dream and save it
    x = x.reshape(img_size)
    x -= random_jitter
    img = deprocess_image(np.copy(x))
    fn = result_prefix + '_at_iteration_%d.png' % i
    imsave(fn, img)

    end_time = time.time()
    print('Image saved as', fn)
    print('Iteration %d completed in %ds' % (i, end_time-start_time))


# In[ ]:




