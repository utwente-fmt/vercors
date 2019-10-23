package hre.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

/**
 * A name space with a single definition per name.
 * 
 * @author Stefan Blom
 *
 * @param <K>
 * @param <D>
 */
public class SingleNameSpace<K,D> implements Map<K,D> {

  private Stack<Map<K,D>> stack=new Stack<Map<K, D>>();
  
  private Map<K,D> map=new HashMap<K, D>();
 
  public void enter(){
    stack.push(map);
    map=new HashMap<K, D>();
    map.putAll(stack.peek());
  }
  public D lookup(K name){
    return map.get(name);
  }
  public void add(K name,D def){
    map.put(name, def);
  }
  public void leave(){
    map=stack.pop();
  }
  
  @Override
  public int size() {
    return map.size();
  }
  @Override
  public boolean isEmpty() {
    return map.isEmpty();
  }
  @Override
  public boolean containsKey(Object key) {
     return map.containsKey(key);
  }
  @Override
  public boolean containsValue(Object value) {
    return map.containsValue(value);
  }
  @Override
  public D get(Object key) {
    return map.get(key);
  }
  @Override
  public D put(K key, D value) {
    return map.put(key, value);
  }
  @Override
  public D remove(Object key) {
    return map.remove(key);
  }
  @Override
  public void putAll(Map<? extends K, ? extends D> m) {
    map.putAll(m);
  }
  @Override
  public void clear() {
    map.clear();
  }
  @Override
  public Set<K> keySet() {
    return map.keySet();
  }
  @Override
  public Collection<D> values() {
    return map.values();
  }
  @Override
  public Set<java.util.Map.Entry<K, D>> entrySet() {
    return map.entrySet();
  }
}
