(ns speclj.components)

(defprotocol SpecComponent
  (install [this description]))

#?(:cljs
   (extend-protocol SpecComponent
     LazySeq
     (install [this description] (doseq [component (seq this)] (install component description)))
     List
     (install [this description] (doseq [component (seq this)] (install component description)))
     EmptyList
     (install [this description] (doseq [component (seq this)] (install component description)))
     PersistentVector
     (install [this description] (doseq [component (seq this)] (install component description)))
     nil
     (install [_this _description] (throw (ex-info (str "Oops!  It looks like you tried to add 'nil' to a spec.  That's probably not what you wanted.") {})))
     object
     (install [_this _description] (comment "Whatever...  Let them pass.")))

   :cljd
   (extend-protocol SpecComponent
     Object
     (install [this description]
       (if (seqable? this)
         (doseq [component (seq this)] (install component description))
         (comment "This prohibits multimethod defs, and other stuff.  Don't be so stingy! Let it pass.")))
     Null
     (install [_this _description] (throw (Exception. (str "Oops!  It looks like you tried to add 'nil' to a spec.  That's probably not what you wanted.")))))

   :default
   (extend-protocol SpecComponent
     Object
     (install [_this _description] (comment "This prohibits multimethod defs, and other stuff.  Don't be so stingy! Let it pass."))
     nil
     (install [_this _description] (throw (Exception. (str "Oops!  It looks like you tried to add 'nil' to a spec.  That's probably not what you wanted."))))
     clojure.lang.Var
     (install [_this _description] (comment "Vars are cool.  Let them pass."))
     clojure.lang.Seqable
     (install [this description] (doseq [component (seq this)] (install component description)))))

(defprotocol IFocusable
  (focused? [_this] "Returns true if the component is focused.")
  (focus! [_this] "Marks the component as focused."))

(defprotocol IChild
  (parent [_this] "Returns the parent of the component"))

(deftype Description [name is-focused? has-focus? ns parent children characteristics tags befores before-alls afters after-alls withs with-alls arounds around-alls]
  SpecComponent
  (install [this description]
    (reset! (.-parent this) description)
    (swap! (.-children ^Description description) conj this))
  IFocusable
  (focused? [_this] @is-focused?)
  (focus! [_this] (reset! is-focused? true))
  IChild
  (parent [_this] @parent)
  Object
  (#?(:cljr ToString :default toString) [_this] (str "Description: " \" name \")))

(defn characteristics [description]
  @(.-characteristics ^Description description))

(defn children [description]
  @(.-children ^Description description))

(defn tags [description]
  @(.-tags ^Description description))

(defn withs [description]
  @(.-withs ^Description description))

(defn with-alls [description]
  @(.-with-alls ^Description description))

(defn befores [description]
  @(.-befores ^Description description))

(defn before-alls [description]
  @(.-before-alls ^Description description))

(defn afters [description]
  @(.-afters ^Description description))

(defn after-alls [description]
  @(.-after-alls ^Description description))

(defn arounds [description]
  @(.-arounds ^Description description))

(defn around-alls [description]
  @(.-around-alls ^Description description))

(defn namespace-of [description]
  (.-ns ^Description description))

(defn enable-focus! [description]
  (reset! (.-has-focus? ^Description description) true))

(defn is-description? [component]
  (instance? Description component))

(defn has-focus? [component]
  (and (is-description? component)
       @(.-has-focus? ^Description component)))

(defn new-description [name is-focused? ns]
  (Description. name (atom is-focused?) (atom false) ns (atom nil) (atom []) (atom []) (atom #{}) (atom []) (atom []) (atom []) (atom []) (atom []) (atom []) (atom []) (atom [])))

(def ^:dynamic *assertions*)
(defn inc-assertions! [] (swap! *assertions* inc))

(deftype Characteristic [name parent body is-focused?]
  SpecComponent
  (install [this description]
    (reset! (.-parent this) description)
    (swap! (.-characteristics ^Description description) conj this))
  IFocusable
  (focused? [_this] @is-focused?)
  (focus! [_this] (reset! is-focused? true))
  IChild
  (parent [_this] @parent)
  Object
  (#?(:cljr ToString :default toString) [_this] (str \" name \")))

(defn new-characteristic
  ([name body is-focused?] (Characteristic. name (atom nil) body (atom is-focused?)))
  ([name description body is-focused?] (Characteristic. name (atom description) body (atom is-focused?))))

(defn is-characteristic? [component]
  (instance? Characteristic component))

(deftype Before [body]
  SpecComponent
  (install [this description]
    (swap! (.-befores ^Description description) conj this)))

(defn new-before [body]
  (Before. body))

(deftype After [body]
  SpecComponent
  (install [this description]
    (swap! (.-afters ^Description description) conj this)))

(defn new-after [body]
  (After. body))

(deftype Around [body]
  SpecComponent
  (install [this description]
    (swap! (.-arounds ^Description description) conj this)))

(defn new-around [body]
  (Around. body))

(deftype BeforeAll [body]
  SpecComponent
  (install [this description]
    (swap! (.-before-alls ^Description description) conj this)))

(defn new-before-all [body]
  (BeforeAll. body))

(deftype AfterAll [body]
  SpecComponent
  (install [this description]
    (swap! (.-after-alls ^Description description) conj this)))

(defn new-after-all [body]
  (AfterAll. body))

(deftype AroundAll [body]
  SpecComponent
  (install [this description]
    (swap! (.-around-alls ^Description description) conj this)))

(defn new-around-all [body]
  (AroundAll. body))

(defprotocol IWith
  (set-value! [_this _value] "Sets the value of the With to something else.")
  (bang! [_this] "Dereferences the With when bang is true."))

(deftype With [name body set-var! value bang]
  SpecComponent
  (install [this description]
    (swap! (.-withs ^Description description) conj this))
  IWith
  (set-value! [_this new-value] (reset! value new-value))
  (bang! [this] (when bang @this))
  #?(:cljs cljs.core/IDeref :cljd cljd.core/IDeref :default clojure.lang.IDeref)
  (#?(:cljs -deref :default deref) [_this]
    (when (= ::none @value)
      (reset! value (body)))
    @value))

(defn reset-with [with]
  (set-value! with ::none)
  (bang! with))

(defn new-with [name body set-var! bang]
  (let [with (With. name body set-var! (atom ::none) bang)]
    (bang! with)                                            ; TODO - MDM: This is the wrong place to deref.  Should do it in body right after arounds.
    with))

(deftype WithAll [name body set-var! value bang]
  SpecComponent
  (install [this description]
    (swap! (.-with-alls ^Description description) conj this))
  IWith
  (set-value! [_this new-value] (reset! value new-value))
  (bang! [this] (when bang @this))
  #?(:cljs cljs.core/IDeref :cljd cljd.core/IDeref :default clojure.lang.IDeref)
  (#?(:cljs -deref :default deref) [_this]
    (when (= ::none @value)
      (reset! value (body)))
    @value))

(defn new-with-all [name body set-var! bang]
  (let [with-all (WithAll. name body set-var! (atom ::none) bang)]
    (bang! with-all)
    with-all))

(deftype Tag [name]
  SpecComponent
  (install [_this description]
    (swap! (.-tags ^Description description) conj name)))

(defn new-tag [name]
  (Tag. name))

(defprotocol IBody
  (body-of [_component] "Returns the body of the component"))

(extend-protocol IBody
  Characteristic (body-of [component] (.-body component))
  With (body-of [component] (.-body component))
  WithAll (body-of [component] (.-body component))
  After (body-of [component] (.-body component))
  AfterAll (body-of [component] (.-body component))
  Before (body-of [component] (.-body component))
  BeforeAll (body-of [component] (.-body component))
  Around (body-of [component] (.-body component))
  AroundAll (body-of [component] (.-body component)))

(defprotocol IName
  (name-of [_component] "Returns the name of the component"))

(extend-protocol IName
  Description (name-of [component] (.-name component))
  Characteristic (name-of [component] (.-name component))
  With (name-of [component] (.-name component))
  WithAll (name-of [component] (.-name component))
  Tag (name-of [component] (.-name component)))
