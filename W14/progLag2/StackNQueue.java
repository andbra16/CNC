import java.util.*;


public class StackNQueue {
	public static void main(String[] args) {
		 Queue queue = new Queue();
		 Stack stack = new Stack();
		 String pala = "wottow";
		 
		 System.out.println(palindrome(queue, stack, pala));
	}
	
	static public boolean palindrome(Queue queue, Stack stack, String pala) {
		for (int i=0; i<pala.length(); i++) {
			 queue.enqueue(pala.charAt(i));
			 stack.push(pala.charAt(i));
		 }
		for (int i=0; i<pala.length(); i++) {
			 Object q = queue.dequeue();
			 Object s = stack.pop();
			 
			 if(q != s) {
				 return false;
			 }
		 }
		return true;
	}
}

class Stack {
	ArrayList<Object> s;
	
	Stack() {
		this.s = new ArrayList<Object>();
	}
	
	public void push(Object j) {
		this.s.add(0, j);
	}
	
	public Object pop() {
		Object item;
		if (this.s.isEmpty()) {
			throw new RuntimeException("Nothing to remove from an empty queue");
		} else {
			item = this.s.get(0);
			this.s.remove(0);
		}
		return item;
	}
	
	public boolean isEmpty() {
		return this.s.isEmpty();
	}
}

class Queue {
	ArrayList<Object> q;
	
	Queue() {
		this.q = new ArrayList<Object>();
	}
	
	public void enqueue(Object j) {
		this.q.add(0, j);
	}
	
	public Object dequeue() {
		Object item;
		if (this.q.isEmpty()) {
			throw new RuntimeException("Nothing to remove from an empty queue");
		} else {
			item = this.q.get(q.size() -1);
			this.q.remove(q.size()-1);
		}
		return item;
	}
	
	public boolean isEmptyQueue() {
		return this.q.isEmpty();
	}
}

