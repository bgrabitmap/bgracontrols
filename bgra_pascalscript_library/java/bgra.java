public class bgra {
/////////////////////// the bgra library declarations ///////////////	
  public static native void create(int num);
  public static native void createwithsize(int num, int left, int top);
  
  public static native void fill(int num, int color);
  public static native int rgb(int r, int g, int b);
  public static native void filtersmartzoom3(int num, int typ);
  public static native void savetofile(int num, String fn);
  public static native void destroy(int num);
  
}
 
