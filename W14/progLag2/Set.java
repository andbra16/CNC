import java.util.*;

public class Set {
  private ArrayList<Object> s;
  
  Set() {
    this.s = new ArrayList<Object>();
  }
  
  public boolean member(Object e) {
    for (int i=0; i<this.s.size(); i++) {
         if (this.s.get(i) == e) {
           return true;
         }
     }
    return false;
  }
  
  public void insert(Object e) {
    for (int i=0; i<this.s.size(); i++) {
		if (this.s.get(i) == e) {
			this.s.add(e);
    	}
  	}
  }
  
  public void remove(Object e) {
    for (int i=0; i<this.s.size(); i++) {
         if (this.s.get(i) == e) {
           this.s.remove(i);
         }
    }
  }
}
      
         
