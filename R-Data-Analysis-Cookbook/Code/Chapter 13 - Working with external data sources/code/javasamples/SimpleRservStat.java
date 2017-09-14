package javasamples;

import org.rosuda.REngine.*;
import org.rosuda.REngine.Rserve.*;
import java.util.Arrays;

public class SimpleRservStat {
    public static void main(String args[]) throws RserveException {
    // Start R session.
    RConnection rc = new RConnection();
    try {
       rc.assign("values", new double[] {1000.5, 1200, 1500.25, 1800, 2100});
       REXP mean = rc.eval("(mean(values))");
       double d = mean.asDouble();
       System.out.println(d);

       double[] range = rc.eval("range(values)").asDoubles();
       System.out.println(Arrays.toString(range));
       rc.close();
    }
    catch (REngineException ree) {
            System.out.println("REngineException ...");
            System.out.println(ree.getMessage());
            rc.close();
        } catch (Exception e) {
            System.out.println("Exception ...");
            System.out.println(e.getMessage());
            rc.close();
        }
    }
}
