package javasamples;

import org.rosuda.JRI.REXP;
import org.rosuda.JRI.Rengine;
import org.rosuda.REngine.REngineException;

public class SimpleJRIStat {
    public static void main(String args[]) throws REngineException
    {
       // Start R session.
       Rengine re = new Rengine (new String[]{"--no-save"}, false, null);

       // Check if the R session is active
       if (!re.waitForR()) {
           return;
       }

       re.assign("values", new double[] {1000.5, 1200, 1500.25, 1800, 2100});
       REXP mean = re.eval("(mean(values))");
       double d = mean.asDouble();
       System.out.println(d);

       re.end();
    }
}
