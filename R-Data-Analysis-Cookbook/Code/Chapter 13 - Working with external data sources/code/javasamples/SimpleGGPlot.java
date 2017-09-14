package javasamples;

import java.awt.*;
import org.rosuda.REngine.*;
import org.rosuda.REngine.Rserve.*;

import java.awt.FlowLayout;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

public class SimpleGGPlot {
    public static void main(String args[]) throws RserveException {
    // Start R session.
    RConnection rc = new RConnection();
    String wd = ".";
    try {
       rc.eval("library(ggplot2)");
       rc.eval("require(ggplot2)");

       if ((args == null) || (args.length == 0)
          wd = System.getProperty("user.dir");
       else
          wd = args[0];
       rc.eval(String.format("wd <- '%s'", wd));
       rc.eval("setwd(wd)");
       System.out.println("Working directory = "+rc.eval("getwd()").asString());

       rc.eval("autonew <- read.csv('auto-mpg.csv',header=TRUE)");
       REXP count = rc.eval("(n=nrow(auto))");
       if (count==null) {
	  System.out.println("Make sure the file auto-mpg.csv file exists in your working directory and you entered a correct value as an argument during execution");
	  System.exit(0);
       }
       rc.eval("png(file='autonew.png',width=400,height=400,res=72)");
       rc.parseAndEval("print(ggplot(data=autonew,aes(x=weight,y=mpg))+geom_point())");
       rc.eval("dev.off()");
      
       System.out.println("Plotting Done");
       REXP xp = rc.parseAndEval("r=readBin('autonew.png','raw',1024*1024)");
       rc.parseAndEval("unlink('autonew.png'); r");
       Image img = Toolkit.getDefaultToolkit().createImage(xp.asBytes());
        ImageIcon icon=new ImageIcon(img);
        JFrame frame=new JFrame();
        frame.setLayout(new FlowLayout());
        frame.setSize(500,500);
        JLabel lbl=new JLabel();
        lbl.setIcon(icon);
        frame.add(lbl);
        frame.setVisible(true);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
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
