import java.util.*;

public class TypeMap extends HashMap<Variable, Type> { 

	public static TypeMap typing (Declarations d) {
  		TypeMap map = new TypeMap( );
  		for (Declaration di : d) {
      		map.put (di.v, di.t);
  		}
  		return map;
	}

	public void display() {
		System.out.println(this);
	}
}
