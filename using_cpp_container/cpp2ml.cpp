#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <map>

extern "C" {

#define Val_none Val_int(0)

static value
Val_some( value v )
{   
    CAMLparam1(v);
    CAMLlocal1(some);
    some = caml_alloc(1, 0);
    Store_field(some, 0, v);
    CAMLreturn(some);
}

static struct custom_operations int_value_map_ops = {
    "maxime.ransan.std::map<int, value*>",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

typedef std::map<int, value*>  IntMap; 
typedef IntMap::iterator       IntMapItr;

#define Map_val(v) (*((IntMap**) Data_custom_val(v)))

static value int_value_map_alloc()
{
    value v = alloc_custom(&int_value_map_ops, sizeof(IntMap*), 0, 1);
    Map_val(v) = new IntMap();
    return v;
}

value int_value_map_empty(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(int_value_map_alloc());
}

value int_value_map_is_empty(value map)
{
    CAMLparam1(map);
    CAMLlocal1(size); 

    IntMap* map_ = Map_val(map); 
    size = Val_bool(map_->size() == 0); 

    CAMLreturn(size);
}

value int_value_map_add(value map, value key, value v) {

    CAMLparam3(map, key, v); 

    value *v_prime = (value *)malloc(sizeof(v_prime)) ; 
    *v_prime = v; 
    caml_register_global_root(v_prime);
    
    int key_ = Int_val(key); 
    std::pair<IntMapItr, bool> r = Map_val(map)->insert(std::make_pair(key_, v_prime)); 

    if(false == r.second) {
       value *v_previous = (r.first->second); 
       caml_remove_global_root(v_previous); 
       free(v_previous); 
       r.first->second = v_prime;
    }

    CAMLreturn(Val_unit);
}

value int_value_map_get(value map, value key) {

    CAMLparam2(map, key); 
    CAMLlocal1(v); 

    int key_ = Int_val(key); 
    std::map<int, value*>* map_ = Map_val(map);
    std::map<int,value*>::iterator i = map_->find(key_); 
    if(map_->end() == i) {
        v = Val_none; 
    }else {
        v = Val_some(*(i->second));
    }
    CAMLreturn(v);
}
value int_value_map_exist(value map, value key) {

    CAMLparam2(map, key); 
    CAMLlocal1(v); 

    int key_ = Int_val(key); 
    std::map<int, value*>* map_ = Map_val(map);
    std::map<int,value*>::iterator i = map_->find(key_); 
    if(map_->end() == i) {
        v = Val_false;
    }else {
        v = Val_true;
    }
    CAMLreturn(v);
}

value int_value_map_size(value map) {
    CAMLparam1(map); 
    CAMLlocal1(size); 
    
    size = Val_int(Map_val(map)->size()); 
    
    CAMLreturn(size);
}

} // extern "C" 

