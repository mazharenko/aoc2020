//
// https://adventofcode.com/2020/day/4
//


#if !INTERACTIVE
module Puzzle4_2
#else
#load "common.fs"
#r "nuget:FParsec, Version=1.1.1"
#endif


open System
open System.IO
open FParsec

let input =
    let text = File.ReadAllText (Path.Combine(__SOURCE_DIRECTORY__, "puzzle4.txt"))
    text.Split "\n\n"
    
let (|BetweenInclusive|_|) min max x =
  if min <= x && x <= max then Some () else None
  

type EyeColor =
    | Amb
    | Blu
    | Brn
    | Gry
    | Grn
    | Hzl
    | Oth
    
type HairColor = | HairColor of string

module BirthYear =
    type BirthYear = private BirthYear of int
    
    let fromInt i =
       match i with
       | BetweenInclusive 1920 2002 -> BirthYear i |> Result.Ok
       | _ -> Result.Error "out of valid range"
       
module IssueYear =
    type IssueYear = private IssueYear of int
    
    let fromInt i =
       match i with
       | BetweenInclusive 2010 2020 -> IssueYear i |> Result.Ok
       | _ -> Result.Error "out of valid range"
       
module ExpirationYear =
    type ExpirationYear = private ExpirationYear of int
    
    let fromInt i =
       match i with
       | BetweenInclusive 2020 2030 -> ExpirationYear i |> Result.Ok
       | _ -> Result.Error "out of valid range"
       
[<Measure>] type cm
[<Measure>] type inch

module Height =
    type Height =
        private
        | HeightCm of int<cm>
        | HeightIn of int<inch>
    let fromCm (x: int<cm>)  =
        match x with
        | BetweenInclusive 150<cm> 193<cm> -> HeightCm x |> Result.Ok
        | _ -> Result.Error "out of valid range"
        
    let fromIn (x: int<inch>) = 
        match x with
        | BetweenInclusive 59<inch> 76<inch> -> HeightIn x |> Result.Ok
        | _ -> Result.Error "out of valid range"
    

type PassportId = | PassportId of string
type CountryId = | CountryId of string

type Attr =
    | EyeColorAttr of EyeColor
    | HairColorAttr of HairColor
    | BirthYearAttr of BirthYear.BirthYear
    | IssueYearAttr of IssueYear.IssueYear
    | ExpirationYearAttr of ExpirationYear.ExpirationYear
    | HeightAttr of Height.Height
    | PassportIdAttr of PassportId
    | CountryIdAttr of CountryId
    
let parserUnwrapResult (p: Parser<Result<'a, string>, 's>) : Parser<'a, 's> =
    fun stream ->
        let result = p stream
        match result.Status with
        | ReplyStatus.Ok ->
            match result.Result with
            | Result.Ok value -> Reply value
            | Result.Error error -> Reply (Error, messageError  error)
        | _ -> Reply (result.Status, result.Error)
        
    
let byr =
    pstring "byr:"
    >>. pint32 |>> BirthYear.fromInt
    |> parserUnwrapResult |>> BirthYearAttr
    
let iyr =
    pstring "iyr:"
    >>. pint32 |>> IssueYear.fromInt
    |> parserUnwrapResult |>> IssueYearAttr
    
let eyr =
    pstring "eyr:"
    >>. pint32 |>> ExpirationYear.fromInt
    |> parserUnwrapResult |>> ExpirationYearAttr
    
let hgt = 
    pstring "hgt:" >>. (
        attempt (pint32 .>> pstring "in" |>> (*)1<inch> |>> Height.fromIn)
        <|>
        (pint32 .>> pstring "cm" |>> (*)1<cm> |>> Height.fromCm) 
    ) |> parserUnwrapResult |>> HeightAttr

let hcl =
    pstring "hcl:#"
    >>. parray 6 (anyOf "0123456789abcdef")
    |>> string
    |>> HairColor |>> HairColorAttr

let ecl =
    pstring "ecl:"
    >>. (
         stringReturn "amb" Amb
         <|> stringReturn "blu" Blu
         <|> stringReturn "brn" Brn
         <|> stringReturn "gry" Gry
         <|> stringReturn "grn" Grn
         <|> stringReturn "hzl" Hzl
         <|> stringReturn "oth" Oth
    ) |>> Attr.EyeColorAttr
        
let pid =
    pstring "pid:"
    >>? parray 9 digit
    |>> string
    |>> PassportId |>> PassportIdAttr
    
let cid =
    pstring "cid:"
    >>? manySatisfy (not << Char.IsWhiteSpace)
    |>> CountryId |>> CountryIdAttr
    
    
let validAttrList (attrs: Attr list) : Parser<_,_> =
    let valid =
        attrs
        |> List.choose (fun attr ->
                    match attr with
                    | EyeColorAttr _ -> Some 0
                    | HairColorAttr _ -> Some 1
                    | BirthYearAttr _ -> Some 2
                    | IssueYearAttr _ -> Some 3
                    | ExpirationYearAttr _ -> Some 4
                    | HeightAttr _ -> Some 5
                    | PassportIdAttr _-> Some 6
                    | CountryIdAttr _-> None
        )
        |> List.sort
        |> (=)[0..6]
    if (valid) then fun _ -> Reply attrs
    else fun _ -> Reply (Error, messageError "invalid attr list")

let attrList =
    (spaces >>.
        sepEndBy1
            (
                byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ecl <|> pid <|> cid
            ) spaces1
        .>> eof) 
    >>= validAttrList

let test p str =
    printfn "%O" (run p str)
    
let check parser s =
    match run parser s with
    | Success _ -> true
    | _ -> false
    
let puzzle4_2 =
    input
    |> Array.map (run attrList)
    |> Array.filter (function | Success _ -> true | _ -> false)
    |> Array.length
     