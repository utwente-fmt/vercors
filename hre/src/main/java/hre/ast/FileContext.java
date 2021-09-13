package hre.ast;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;

import static hre.lang.System.*;

public class FileContext {
  private ArrayList<String> list=new ArrayList<String>();
  private ArrayList<Integer> offset=new ArrayList<Integer>();
  private final FileSwingContext gui;
  
  public FileContext(Path filePath, boolean use_gui){
    if (use_gui) {
      gui=new FileSwingContext();  
    } else {
      gui=null;
    }
    try {
      
      BufferedReader in=new BufferedReader(new FileReader(filePath.toFile()));
      String line;
      int pos=0;
      while((line=in.readLine())!=null){
        list.add(line);
        try {
          if(use_gui){
            pos=gui.doc.getLength();
            offset.add(pos);
            gui.doc.insertString(pos, line + "\n" ,gui.doc.getStyle("regular"));
          }
          } catch (BadLocationException ble) {
            Debug("Couldn't insert initial text into text pane.");
          }
      }
      in.close();
      if (use_gui) SwingUtilities.invokeLater(new Runnable() {
        public void run() {
          //Turn off metal's use of bold fonts
          UIManager.put("swing.boldMetal", Boolean.FALSE);
          JFrame frame = new JFrame("TextSamplerDemo");
          frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
          //Add content to the window.
          frame.add(gui);
          //Display the window.
          frame.pack();
          frame.setVisible(true);
        }
      });
    } catch (IOException e){
      Abort("Could not create context for %s: %s",filePath,e);
    }
    
  }
  
  public String getLine(int i){
    return list.get(i-1);
  }
  public void printContext(FileOrigin o,int before, int after){
    if (gui!=null){
      final StyleContext cont = StyleContext.getDefaultStyleContext();
      final AttributeSet attr = cont.addAttribute(cont.getEmptySet(),
          StyleConstants.Background, Color.RED);
      int begin=offset.get(o.getFirstLine()-1)+o.getFirstColumn()-1;
      int end=offset.get(o.getLastLine()-1)+o.getLastColumn();
      gui.doc.setCharacterAttributes(begin,end-begin, attr ,false);
    }
    int N=o.getFirstLine()-before;
    if (N<1) N=1;
    int K=o.getLastLine();
    if (K<0) K=o.getFirstLine();
    K=K+after;
    if (K>list.size()) K=list.size();
    int len=1;
    String currentLine;
    for(int i=N;i<=K;i++){
      String line=getLine(i);
      if (i==o.getFirstLine()){
        currentLine = "";
        int C=o.getFirstColumn();
        currentLine += "    ";
        int k=1;
        while(k<C) {
          currentLine += " ";
          k++;
        }
        currentLine += "[";
        if (o.getLastLine()==i){
          C=o.getLastColumn();
        } else {
          C=line.length();
        }
        while(k<=C){
          currentLine += "-";
          k++;         
        }
        Output(currentLine);
      }
      Output("%4d %s",i,line);
      if (line.length()>len) len=line.length();
      if (i==o.getLastLine()){
        int C;
        currentLine = "";
        if (o.getFirstLine() == i){
          C=o.getFirstColumn();
        } else {
          C=1;
        }
        currentLine += "     ";
        int k=1;
        while(k<C) {
          currentLine += " ";
          k++;
        }
        C=o.getLastColumn();
        while(k<=C){
          currentLine += "-";
          k++;         
        }
        currentLine += "]";
        Output(currentLine);
      }
    }
  }

  public void mark(FileOrigin o, String result) {
    if (gui!=null){
      Color color;
      switch(result){
      case "green": color=Color.green; break;
      case "red": color=Color.red; break;
      default : color=Color.white; break;
      }
      final StyleContext cont = StyleContext.getDefaultStyleContext();
      final AttributeSet attr = cont.addAttribute(cont.getEmptySet(), StyleConstants.Background, color);
      int begin=offset.get(o.getFirstLine()-1)+o.getFirstColumn()-1;
      int end=offset.get(o.getLastLine()-1)+o.getLastColumn();
      gui.doc.setCharacterAttributes(begin,end-begin, attr ,false);
    }
  }
  
}
