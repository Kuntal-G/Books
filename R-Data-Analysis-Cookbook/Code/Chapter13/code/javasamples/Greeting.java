// Thie java code returns Greeting 
package javasamples;
 
public class Greeting {

    String greeting = "Hi World!";
 
    public String getString(String name) {
	if (name!=null && !name.equals(""))
           greeting =  "Hello " + name;
        else
	   greeting = "Hello there!";
	
	return greeting;
    }

    public String toString() {
	return greeting;
    }

    public static void main(String[] args) {
 
    }
}
