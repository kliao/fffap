#load "Tree.fs"
open Tree

type CurveType = Volatility | Dividends | Rates | FundingSpread
type FlatParameter = {flatValue: float; curveType: CurveType}
type CurveParameter = {tag: string; curveType: CurveType}

type ParameterContents =
    | Flat of FlatParameter
    | Curve of CurveParameter

type ParameterDecoration =
    | Token of string
    | Symbol of string
    | Holding of string

type Context = Tree<ParameterContents,ParameterDecoration>

let fromFlat flat =
    LeafNode (Flat flat)

let fromCurve curve =
    LeafNode (Curve curve)

let assignToToken token parameter =
    let container = Token token
    InternalNode (container, [parameter])
    
let assignToSymbol symbol parameter =
    let container = Symbol symbol
    InternalNode (container, [parameter])
    
let assignToHolding holding parameter =
    let container = Holding holding
    InternalNode (container, [parameter])

let flatVol = {flatValue=0.20; curveType=Volatility}
let divCurve = {tag="current"; curveType=Dividends}

let ibmContext =
    flatVol
    |> fromFlat
    |> assignToSymbol "IBM"

let holdingContext =
    divCurve
    |> fromCurve
    |> assignToHolding "12345"

let mainContext =
    let container = Token "blue"
    InternalNode (container, [ibmContext; holdingContext])


let description context =

    let fLeaf leafData = 
        match leafData with
        | Flat f ->
            sprintf "flat %A at %f" f.curveType f.flatValue
        | Curve c ->
            sprintf "%A curve" c.curveType

    let fNode nodeData innerTexts = 
        let innerText = String.concat " & " innerTexts 
        match nodeData with
        | Token t ->
            sprintf "%s under token %s" innerText t
        | Symbol s ->
            sprintf "%s for symbol %s" innerText s
        | Holding h ->
            sprintf "%s for holding %s" innerText h

    // main call
    Tree.cata fLeaf fNode context

mainContext |> description