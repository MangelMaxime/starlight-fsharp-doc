/// <summary>Generic types and functions used to test generic constraint extraction.</summary>
module Reference.Generics

open System

// ---------------------------------------------------------------------------
// Simple generic types
// ---------------------------------------------------------------------------

/// <summary>A generic box that holds any type.</summary>
/// <typeparam name="T">The type of the contained value.</typeparam>
type Box<'T> =
    {
        /// <summary>The value inside the box.</summary>
        Value: 'T
    }

/// <summary>A generic result type that can hold either a value or an error.</summary>
/// <typeparam name="T">The type of the success value.</typeparam>
/// <typeparam name="E">The type of the error value.</typeparam>
type Result<'T, 'E> =
    | Success of value: 'T
    | Failure of error: 'E

/// <summary>A generic tree structure with values at the leaves.</summary>
/// <typeparam name="T">The type of values stored in the tree.</typeparam>
type Tree<'T> =
    | Leaf of value: 'T
    | Node of left: Tree<'T> * right: Tree<'T>

// ---------------------------------------------------------------------------
// Generic functions
// ---------------------------------------------------------------------------

/// <summary>Wraps a value in a Box.</summary>
/// <typeparam name="T">The type of the value.</typeparam>
/// <param name="value">The value to wrap.</param>
/// <returns>A Box containing the value.</returns>
let box<'T> (value: 'T) : Box<'T> =
    {
        Value = value
    }

/// <summary>Extracts the value from a Box.</summary>
/// <typeparam name="T">The type of the value.</typeparam>
/// <param name="b">The box to unwrap.</param>
/// <returns>The contained value.</returns>
let unbox<'T> (b: Box<'T>) : 'T = b.Value

/// <summary>Maps a function over a Box.</summary>
/// <typeparam name="T">The input type.</typeparam>
/// <typeparam name="U">The output type.</typeparam>
/// <param name="f">The mapping function.</param>
/// <param name="b">The box to map over.</param>
/// <returns>A new Box with the mapped value.</returns>
let mapBox<'T, 'U> (f: 'T -> 'U) (b: Box<'T>) : Box<'U> =
    {
        Value = f b.Value
    }

/// <summary>Creates a singleton tree.</summary>
/// <typeparam name="T">The type of the value.</typeparam>
/// <param name="value">The value for the leaf.</param>
/// <returns>A Tree containing just the value.</returns>
let singleton<'T> (value: 'T) : Tree<'T> = Leaf value

/// <summary>Maps a function over all values in a tree.</summary>
/// <typeparam name="T">The input type.</typeparam>
/// <typeparam name="U">The output type.</typeparam>
/// <param name="f">The mapping function.</param>
/// <param name="tree">The tree to map over.</param>
/// <returns>A new Tree with mapped values.</returns>
let rec mapTree<'T, 'U> (f: 'T -> 'U) (tree: Tree<'T>) : Tree<'U> =
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(mapTree f l, mapTree f r)

/// <summary>Counts the number of leaves in a tree.</summary>
/// <typeparam name="T">The type of values in the tree (unused but required for type inference).</typeparam>
/// <param name="tree">The tree to count.</param>
/// <returns>The number of leaf nodes.</returns>
let rec countLeaves<'T> (tree: Tree<'T>) : int =
    match tree with
    | Leaf _ -> 1
    | Node(l, r) -> countLeaves l + countLeaves r

// ---------------------------------------------------------------------------
// Generic constraints
// ---------------------------------------------------------------------------

/// <summary>Finds the maximum value in a tree. Requires the type to be comparable.</summary>
/// <typeparam name="T">The type of values - must implement IComparable.</typeparam>
/// <param name="tree">The tree to search.</param>
/// <returns>The maximum value.</returns>
let rec findMax<'T when 'T: comparison> (tree: Tree<'T>) : 'T =
    match tree with
    | Leaf v -> v
    | Node(l, r) ->
        let maxL = findMax l
        let maxR = findMax r

        if maxL > maxR then
            maxL
        else
            maxR

/// <summary>Sums all numeric values in a tree using generic math.</summary>
/// <typeparam name="T">The numeric type - must have static member (+).</typeparam>
/// <param name="tree">The tree to sum.</param>
/// <returns>The sum of all values.</returns>
let inline sumTree< ^T when ^T: (static member (+): ^T * ^T -> ^T) and ^T: (static member Zero: ^T)>
    (tree: Tree< ^T >)
    : ^T
    =
    let rec loop t acc =
        match t with
        | Leaf v -> v + acc
        | Node(l, r) -> loop r (loop l acc)

    loop tree LanguagePrimitives.GenericZero< ^T>

/// <summary>Finds an item in a tree by predicate.</summary>
/// <typeparam name="T">The type of values in the tree.</typeparam>
/// <param name="predicate">The function to test items.</param>
/// <param name="tree">The tree to search.</param>
/// <returns>Some value if found, None otherwise.</returns>
let rec tryFind<'T> (predicate: 'T -> bool) (tree: Tree<'T>) : 'T option =
    match tree with
    | Leaf v ->
        if predicate v then
            Some v
        else
            None
    | Node(l, r) ->
        match tryFind predicate l with
        | Some v -> Some v
        | None -> tryFind predicate r

/// <summary>Disposes all IDisposable values in a tree.</summary>
/// <typeparam name="T">The type of values - must implement IDisposable.</typeparam>
/// <param name="tree">The tree containing disposable values.</param>
let rec disposeAll<'T when 'T :> IDisposable> (tree: Tree<'T>) : unit =
    let rec loop (t: Tree<'T>) =
        match t with
        | Leaf(v: 'T) -> v.Dispose()
        | Node(l, r) ->
            loop l
            loop r

    loop tree

/// <summary>Formats all values in a tree to strings.</summary>
/// <typeparam name="T">The type of values - must override ToString.</typeparam>
/// <param name="tree">The tree to format.</param>
/// <returns>A tree of string representations.</returns>
let formatTree<'T when 'T :> obj> (tree: Tree<'T>) : Tree<string> =
    mapTree (fun x -> x.ToString()) tree

/// <summary>Clones a tree of cloneable items.</summary>
/// <typeparam name="T">The type of values - must implement ICloneable.</typeparam>
/// <param name="tree">The tree to clone.</param>
/// <returns>A new tree with cloned values.</returns>
let rec cloneTree<'T when 'T :> ICloneable> (tree: Tree<'T>) : Tree<'T> =
    match tree with
    | Leaf(v: 'T) -> Leaf(v.Clone() :?> 'T)
    | Node(l, r) -> Node(cloneTree l, cloneTree r)

/// <summary>Equality constraint demonstration.</summary>
/// <typeparam name="T">The type that must support equality.</typeparam>
/// <param name="a">First value.</param>
/// <param name="b">Second value.</param>
/// <returns>True if equal.</returns>
let areEqual<'T when 'T: equality> (a: 'T) (b: 'T) : bool = a = b

/// <summary>Unmanaged constraint for efficient copying.</summary>
/// <typeparam name="T">The unmanaged struct type.</typeparam>
/// <param name="value">The value to work with.</param>
/// <returns>The value unchanged (demonstration only).</returns>
let unmanagedDemo<'T when 'T: unmanaged and 'T: struct> (value: 'T) : 'T = value

/// <summary>Not null constraint demonstration.</summary>
/// <typeparam name="T">The type that must not be null.</typeparam>
/// <param name="value">The value to check.</param>
/// <returns>The value if not null.</returns>
let notNullDemo<'T when 'T: not null> (value: 'T) : 'T = value

// ---------------------------------------------------------------------------
// Nested generics
// ---------------------------------------------------------------------------

/// <summary>A generic wrapper around a list.</summary>
/// <typeparam name="T">The element type.</typeparam>
type ListWrapper<'T> =
    {
        /// <summary>The wrapped list.</summary>
        Items: 'T list
    }

/// <summary>Maps over a list wrapper.</summary>
/// <typeparam name="T">The input element type.</typeparam>
/// <typeparam name="U">The output element type.</typeparam>
/// <param name="f">The mapping function.</param>
/// <param name="wrapper">The wrapper to map over.</param>
/// <returns>A new wrapper with mapped items.</returns>
let mapListWrapper<'T, 'U> (f: 'T -> 'U) (wrapper: ListWrapper<'T>) : ListWrapper<'U> =
    {
        Items = List.map f wrapper.Items
    }

/// <summary>Flattens a tree of trees.</summary>
/// <typeparam name="T">The inner type.</typeparam>
/// <param name="tree">The tree of trees.</param>
/// <returns>A flattened tree.</returns>
let rec flattenTree<'T> (tree: Tree<Tree<'T>>) : Tree<'T> =
    match tree with
    | Leaf inner -> inner
    | Node(l, r) -> Node(flattenTree l, flattenTree r)

/// <summary>Creates a tree from a list.</summary>
/// <typeparam name="T">The element type.</typeparam>
/// <param name="items">The items to organize into a tree.</param>
/// <returns>A balanced tree.</returns>
let treeFromList<'T> (items: 'T list) : Tree<'T> =
    let rec build lst =
        match lst with
        | [] -> failwith "Cannot build tree from empty list"
        | [ x ] -> Leaf x
        | xs ->
            let mid = xs.Length / 2
            Node(build xs[.. mid - 1], build xs[mid..])

    build items

// ---------------------------------------------------------------------------
// Mutable values
// ---------------------------------------------------------------------------

/// <summary>A mutable counter for demonstration.</summary>
let mutable counter = 0

/// <summary>A mutable configuration string.</summary>
let mutable configValue = "default"

// ---------------------------------------------------------------------------
// Anonymous records
// ---------------------------------------------------------------------------

/// <summary>Creates a person record.</summary>
/// <param name="name">The person's name.</param>
/// <param name="age">The person's age.</param>
/// <returns>An anonymous record.</returns>
let makePerson (name: string) (age: int) =
    {|
        Name = name
        Age = age
    |}

/// <summary>Gets the name from a person record.</summary>
/// <param name="person">The person record.</param>
/// <returns>The name.</returns>
let getPersonName
    (person:
        {|
            Name: string
            Age: int
        |})
    =
    person.Name

// ---------------------------------------------------------------------------
// Postfix syntax types (list, option, seq, voption, ref, array)
// ---------------------------------------------------------------------------

/// <summary>Finds first matching item in a list.</summary>
/// <typeparam name="T">The element type.</typeparam>
/// <param name="predicate">The predicate to test.</param>
/// <param name="items">The list to search.</param>
/// <returns>The first matching item, or None.</returns>
let tryFindInList<'T> (predicate: 'T -> bool) (items: 'T list) : 'T option =
    List.tryFind predicate items

/// <summary>Filters a sequence.</summary>
/// <typeparam name="T">The element type.</typeparam>
/// <param name="predicate">The predicate to test.</param>
/// <param name="items">The sequence to filter.</param>
/// <returns>A sequence of matching items.</returns>
let filterSeq<'T> (predicate: 'T -> bool) (items: 'T seq) : 'T seq = Seq.filter predicate items

/// <summary>Tries to get a value from a voption.</summary>
/// <typeparam name="T">The value type.</typeparam>
/// <param name="opt">The value option.</param>
/// <returns>The value, or None.</returns>
let voptionToOption<'T> (opt: 'T voption) : 'T option =
    match opt with
    | ValueSome v -> Some v
    | ValueNone -> None

/// <summary>Creates a reference cell.</summary>
/// <typeparam name="T">The value type.</typeparam>
/// <param name="value">The initial value.</param>
/// <returns>A reference cell.</returns>
let makeRef<'T> (value: 'T) : 'T ref = ref value

/// <summary>Creates an array from a list.</summary>
/// <typeparam name="T">The element type.</typeparam>
/// <param name="items">The items to convert.</param>
/// <returns>An array of items.</returns>
let listToArray<'T> (items: 'T list) : 'T[] = Array.ofList items

// ---------------------------------------------------------------------------
// Generic higher-order functions
// ---------------------------------------------------------------------------

/// <summary>Folds a tree from left to right.</summary>
/// <typeparam name="T">The tree element type.</typeparam>
/// <typeparam name="S">The accumulator type.</typeparam>
/// <param name="folder">The folding function.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The tree to fold.</param>
/// <returns>The final accumulated value.</returns>
let rec foldTree<'T, 'S> (folder: 'S -> 'T -> 'S) (state: 'S) (tree: Tree<'T>) : 'S =
    match tree with
    | Leaf v -> folder state v
    | Node(l, r) ->
        let stateL = foldTree folder state l
        foldTree folder stateL r

/// <summary>Zips two trees together.</summary>
/// <typeparam name="T">The first tree's element type.</typeparam>
/// <typeparam name="U">The second tree's element type.</typeparam>
/// <param name="tree1">The first tree.</param>
/// <param name="tree2">The second tree.</param>
/// <returns>A tree of tuples.</returns>
let rec zipTrees<'T, 'U> (tree1: Tree<'T>) (tree2: Tree<'U>) : Tree<'T * 'U> =
    match tree1, tree2 with
    | Leaf v1, Leaf v2 -> Leaf(v1, v2)
    | Node(l1, r1), Node(l2, r2) -> Node(zipTrees l1 l2, zipTrees r1 r2)
    | _ -> failwith "Tree structures must match"
