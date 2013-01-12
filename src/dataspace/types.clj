(ns dataspace.types)

(def primitive-types  
  #{Boolean/TYPE  Character/TYPE Byte/TYPE  Short/TYPE  Integer/TYPE  Long/TYPE  Float/TYPE  Double/TYPE boolean char byte short int long float double})

(def ^:dynamic  *types*
  (atom {:boolean Boolean/TYPE
         :char Character/TYPE
         :byte Byte/TYPE
         :short Short/TYPE
         :int Integer/TYPE
         :long Long/TYPE
         :float Float/TYPE
         :double Double/TYPE
         boolean Boolean/TYPE
         char Character/TYPE
         byte Byte/TYPE
         short Short/TYPE
         int Integer/TYPE
         long Long/TYPE
         float Float/TYPE
         double Double/TYPE
         Boolean/TYPE Boolean/TYPE
         Character/TYPE Character/TYPE
         Byte/TYPE Byte/TYPE 
         Short/TYPE Short/TYPE  
         Integer/TYPE Integer/TYPE 
         Long/TYPE Long/TYPE 
         Float/TYPE Float/TYPE 
         Double/TYPE Double/TYPE
         }))

(def ^:dynamic *type-bytesize*
  (atom {Boolean/TYPE 8 ;;XXX Need to double check that storage for booleans is 8 bytes
         Character/TYPE Character/SIZE
         Byte/TYPE Byte/SIZE 
         Short/TYPE Short/SIZE  
         Integer/TYPE Integer/SIZE 
         Long/TYPE Long/SIZE 
         Float/TYPE Float/SIZE 
         Double/TYPE Double/SIZE}))

(defn resolve-type [t]
  (@*types* t))

(defn resolve-type-size [t]
  (get type-bytesize (resolve-type t) 64))

(defn required-storage-size [type count]
  (*  (/ (resolve-type-size type) Byte/SIZE)  count))

