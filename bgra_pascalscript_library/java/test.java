import static java.lang.System.out;
import javax.swing.JOptionPane;

public class test {

 public static void main(String[] args) 
  {
	   System.loadLibrary("bgrabitmap");
   
   out.println("Library is loaded...");
   out.println("______________________________________________________________________");  
   out.println(); 

  bgra.createwithsize(0,100,100);
  bgra.fill(0, bgra.rgb(0, 255, 0));
  bgra.savetofile(0, "test.png");
  bgra.destroy(0); 
   out.println("Bitmap is saved...");
   out.println("______________________________________________________________________");  
   out.println(); 
 
  }
}
