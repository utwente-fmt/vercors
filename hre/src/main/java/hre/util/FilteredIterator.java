package hre.util;

import java.util.Iterator;

/**
 * Filters the given iterator for the elements that must be passed.
 * @author sccblom
 *
 * @param <E>
 */
public class FilteredIterator<F,E> implements Iterator<E> {
  /**
   * Buffer the next non-null element to be returned.
   */
  private E buffer;
  /**
   * The iterator that generates candidates.
   */
  private Iterator<F> iter;
  /**
   * The filter that decides which candidates must be passed.
   */
  private Function<F,E> filter;
  
  /**
   * Search the given iterator for the next element to be passed and store that
   * element in buffer. 
   */
  private void fill_buffer(){
    while(iter.hasNext()){
      buffer=filter.apply(iter.next());
      if (buffer!=null) return;
    }
    buffer=null;
  }

  public FilteredIterator(Iterator<F> iter,Function<F,E> filter){
    this.iter=iter;
    this.filter=filter;
    fill_buffer();
  }

  public boolean hasNext() {
    return buffer!=null;
  }

  public E next() {
    E res=buffer;
    fill_buffer();
    return res;
  }

  public void remove() {
    throw new UnsupportedOperationException();    
  }
  
}
