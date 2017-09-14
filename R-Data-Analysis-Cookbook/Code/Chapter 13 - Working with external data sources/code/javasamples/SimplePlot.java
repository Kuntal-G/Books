package javasamples;

import java.awt.*;
import org.rosuda.JRI.REXP;
import org.rosuda.JRI.Rengine;
import org.rosuda.REngine.REngineException;

public class SimplePlot {
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
       if ((args == null) || (args.length == 0))
	  wd = System.getProperty("user.dir");
       else
          wd = args[0];
       re.eval(String.format("wd <- '%s'", wd));
       re.eval("setwd(wd)");
       System.out.println("Working directory = "+re.eval("getwd()").asString());

       //Read auto csv
       re.eval("auto<-read.csv('auto-mpg.csv',head=T)");
       REXP count = re.eval("(n=nrow(auto))");
       if (count==null) {
	  System.out.println("Make sure the file auto-mpg.csv file exists in your working directory and you entered a correct value as an argument during execution");
	  System.exit(0);
       }
       System.out.println("Auto Count " + count.asInt());

       // Plot
       re.eval("png(file='auto.png', width=400, height=400, res=72);");
       re.eval("plot(auto$mpg,auto$weight)");
       re.eval("dev.off()");
       System.out.println("Plotting Done");

       re.end();
    }
}
