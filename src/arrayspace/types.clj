(ns arrayspace.types)

(def ^:dynamic  *types*
  (atom {:boolean Boolean/TYPE
         :char Character/TYPE
         :byte Byte/TYPE
         :short Short/TYPE
         :int Integer/TYPE
         :long Long/TYPE
         :float Float/TYPE
         :double Double/TYPE
         "boolean" Boolean/TYPE
         "char" Character/TYPE
         "byte" Byte/TYPE
         "short" Short/TYPE
         "int" Integer/TYPE
         "long" Long/TYPE
         "float" Float/TYPE
         "double" Double/TYPE
         'Boolean/TYPE Boolean/TYPE
         'Character/TYPE Character/TYPE
         'Byte/TYPE Byte/TYPE
         'Short/TYPE Short/TYPE
         'Integer/TYPE Integer/TYPE
         'Long/TYPE Long/TYPE
         'Float/TYPE Float/TYPE
         'Double/TYPE Double/TYPE
         boolean Boolean/TYPE
         char Character/TYPE
         byte Byte/TYPE
         short Short/TYPE
         int Integer/TYPE
         long Long/TYPE
         float Float/TYPE
         double Double/TYPE
         java.lang.Boolean Boolean/TYPE
         java.lang.Character Character/TYPE
         java.lang.Byte Byte/TYPE
         java.lang.Short Short/TYPE
         java.lang.Integer Integer/TYPE
         java.lang.Long Long/TYPE
         java.lang.Float Float/TYPE
         java.lang.Double Double/TYPE
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

(def ^:dynamic  *type-coercion-fns*
  (atom {Boolean/TYPE boolean
         Character/TYPE char
         Byte/TYPE byte
         Short/TYPE short
         Integer/TYPE int
         Long/TYPE long
         Float/TYPE float
         Double/TYPE double
         java.lang.Boolean boolean
         java.lang.Character char
         java.lang.Byte byte
         java.lang.Short short
         java.lang.Integer int
         java.lang.Long long
         java.lang.Float float
         java.lang.Double double
         boolean boolean
         char char
         byte byte
         short short
         int int
         long long
         float float
         double double}))

(defn resolve-type [t]
  (@*types* t))

(defn resolve-type-size [t]
  (get @*type-bytesize* (resolve-type t) Double/SIZE))

(defn resolve-type-coercion-fn [t]
  (let [true-type (resolve-type t)]
    (or (@*type-coercion-fns* true-type) (@*type-coercion-fns* t))))

(defn required-storage-size [type count]
  (*  (/ (resolve-type-size type) Byte/SIZE) count))
