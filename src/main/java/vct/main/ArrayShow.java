package vct.main;

public class ArrayShow {

  private Object[] array;
  private String sep;
  
  public ArrayShow(String sep,Object ... objects) {
    array=objects;
    this.sep=sep;
  }
  
  public String toString(){
    if (array.length==0) {
      return "";
    } else {
      String res=array[0].toString();
      for(int i=1;i<array.length;i++){
        res+=sep+array[i].toString();
      }
      return res;
    }
  }
}
