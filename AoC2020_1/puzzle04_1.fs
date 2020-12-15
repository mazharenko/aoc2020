//
// https://adventofcode.com/2020/day/4
//

#if !INTERACTIVE
module Puzzle4_1
#else
#load "common.fs"
#r "nuget:FParsec"
#endif

open System.IO
open FParsec


let input =
    let text = File.ReadAllText (Path.Combine(__SOURCE_DIRECTORY__, "puzzle04.txt"))
    text.Split "\n\n"
    
type EyeColor = | EyeColor of string
type HairColor = | HairColor of string
type BirthYear = | BirthYear of string
type IssueYear = | IssueYear of string
type ExpirationYear = | ExpirationYear of string
type Height = | Height of string
type PassportId = | PassportId of string
type CountryId = | CountryId of string

type Attr =
    | EyeColorAttr of EyeColor
    | HairColorAttr of HairColor
    | BirthYearAttr of BirthYear
    | IssueYearAttr of IssueYear
    | ExpirationYearAttr of ExpirationYear
    | HeightAttr of Height
    | PassportIdAttr of PassportId
    | CountryIdAttr of CountryId
    
let attribute code =
    pstring (sprintf "%s:" code)
    >>. manySatisfy (not << System.Char.IsWhiteSpace)
    
    
let byr = attribute "byr" |>> BirthYear |>> BirthYearAttr
let iyr = attribute "iyr" |>> IssueYear |>> IssueYearAttr
let eyr = attribute "eyr" |>> ExpirationYear |>> ExpirationYearAttr
let hgt = attribute "hgt" |>> Height |>> HeightAttr
let hcl = attribute "hcl" |>> HairColor |>> HairColorAttr
let ect = attribute "ecl" |>> EyeColor |>> EyeColorAttr
let pid = attribute "pid" |>> PassportId |>> PassportIdAttr
let cid = attribute "cid" |>> CountryId |>> CountryIdAttr
    
    
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
               byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ect <|> pid <|> cid
            ) spaces1
        .>> eof) 
    >>= validAttrList

let test p str =
    printfn "%O" (run p str)
    
let check parser s =
    match run parser s with
    | Success _ -> true
    | _ -> false
    
let puzzle4_1 =
    input
    |> Seq.filter (check attrList)
    |> Seq.length
     