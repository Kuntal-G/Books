package javasamples;

import org.rosuda.JRI.REXP;
import org.rosuda.JRI.Rengine;

import org.rosuda.REngine.REngineException;
import org.rosuda.REngine.JRI.JRIEngine;

public class InvokeRScript {

 public static void main(String args[]) throws REngineException
 {
       String wd = ".";
       // Start R session.
       Rengine re = new Rengine (new String[]{"--vanilla"}, false, null);

       // Check if the R session is active
       if (!re.waitForR()) {
           return;
       }

       // Set working directory
       if (args.length < 2) {
	  System.out.println("To execute, please provide 2 variable names from auto-mpg dataaset");
	  System.out.println("You can also add an optional 3rd argument for your working directory");
       }

       if (args.length == 2) 
          wd = System.getProperty("user.dir");
       else
          wd = args[2];
       re.eval(String.format("wd <- '%s'", wd));
       re.eval("setwd(wd)");
       System.out.println("Working directory = "+re.eval("getwd()").asString());

       re.assign("var1",args[0]); 
       re.assign("var2",args[1]); 
       re.eval("source('corr.R')");

       REXP cor = re.eval("result");

       System.out.println("Correlation = " + cor.asDouble());
       re.end();
   }
}
