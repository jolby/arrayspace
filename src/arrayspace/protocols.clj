(ns arrayspace.protocols)

(defprotocol Range
  (lower-bound [this] "Return the lower bound of this range")
  (upper-bound [this] "Return the upper bound of this range"))

(defprotocol Domain
  "A Domain represents an index set. Domains are a generalization of the region
   concept pioneered by the ZPL language. Domains can be named, assigned, and
   passed between functions. Domains support iteration, intersection,
   set-oriented queries, and operations for creating other domains. They are
   also used to declare, slice, and reallocate arrays

   A Domain is used to represent each domain value in a program. As such, its
   main responsibility is to store a representation of the domain’s index set.
   For layouts and regular domains, the complete index set representation is
   typically stored directly within the descriptor. For distributions of
   irregular domains that require O(numIndices) storage to represent the index
   set, a distribution will typically store only summarizing information in its
   global descriptor. The representation of the complete index set is spread
   between its associated local descriptors in order to achieve scalability"

  (shape [this] "Return the tuple describing the dimensional shape of this domain")
  (rank [this] "Return the rank of the dimensions"))

(defprotocol DomainMap
  "A domain map speciﬁes the implementation of the domains and arrays that are
   mapped using it. That is, it deﬁnes how domain indices and array elements
   are mapped to distributions, how they are stored in memory, and how operations
   such as accesses, iteration, and slicing are performed. Each domain and
   array is mapped using some domain map."

  (transform-coords [this coords] "Transform the high level Domain coordinates to
  low-level element coords in the Locale/Distribution"))

(defprotocol Distribution
  "A distribution abstractly represents a unit of the target architecture that
   supports processing and storage. On conventional machines, a distribution is
   typically deﬁned to be a single node, its memory, and its multicore/SMP
   processors. 

   Distributions facilitate the storage/retrieval of the actual elements of an
   array. It is the responsibility of the DomainMap to translate the
   coordinates of a domain into the low-level storage coordinates of the
   elements contained in a distribution.

   Many implementations of Distributions will also implement the Indexed and
   Counted protocols for low level access to their elements."
  (descriptor [this] 
    "Return the descriptor containing low-level storage and
  layout information for this Distribution"))

(defprotocol Reshapeable
  ""
  (reshape [this shape]))

(defprotocol MutableReshapeable
  ""
  (reshape! [this shape]))

(defprotocol IndexedAccess
  "Protocol for indexed read access to arrays of any dimensions."    
  (mget [m indexes]))

(defprotocol LinearIndexedAccess
  (get-1d [this ^long indx] 
    "Get the element at the provided index based on a 1d linear address model"))

(defprotocol IndexedMutation
  "Protocol for indexed write access to arrays of any dimensions."
  (mset! [m indexes val]))

(defprotocol LinearIndexedMutation
  "Protocol for indexed write access to arrays of any dimensions."
  (set-1d! [m idx val]))

(defprotocol Sliceable
  "Protocol for taking slices of arrays of any dimensions"
  (slice [this start stop] [start stop])
  (slice [this start stop stride] [start stop stride]))


(defprotocol DataDescriptor
  "Provides LowLevel information (size, byte-layout) of each element in a
Domain's coordinate space. DataDescriptor's are dependent on their Locale to
provide information about low level storage and layout details"
  (element-class [this]
    "returns the class of the elements of x")
  (element-size [this]
    "Size in bytes of each element")
  (data-format [this]
    "#{:row-major :column-major} Order/format elements are layed out in memory: "
    ":row-major (C-lang), :column-major (Fortran)"))

(defprotocol ValueRecord
  "Protocol for pure value types"
  (get-field [this kw] "Return field matched to kw"))

(defprotocol AggregateValueRecord
  "Protocol for pure value type that is part of a linearly addressable sequence of the same type"
  (set-index [this ^long idx]))
