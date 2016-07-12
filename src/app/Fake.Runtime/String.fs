﻿/// Contains basic functions for string manipulation.

module Fake.Runtime.String
type String = System.String
open Fake.Runtime.Environment
open System
open System.IO
open System.Text
open System.Collections.Generic

/// [omit]
let productName() = "FAKE"

/// Returns if the string is null or empty
let inline isNullOrEmpty value = String.IsNullOrEmpty value

/// Returns if the string is not null or empty
let inline isNotNullOrEmpty value = String.IsNullOrEmpty value |> not

/// Returns if the string is null or empty or completely whitespace
let inline isNullOrWhiteSpace value = isNullOrEmpty value || value |> Seq.forall Char.IsWhiteSpace

/// Replaces the given pattern in the given text with the replacement
let inline replace (pattern : string) replacement (text : string) = text.Replace(pattern, replacement)

/// Converts a sequence of strings to a string with delimiters
let inline separated delimiter (items : string seq) = String.Join(delimiter, Array.ofSeq items)

/// Removes the slashes from the end of the given string
let inline trimSlash (s : string) = s.TrimEnd('\\')

/// Splits the given string at the given char delimiter
let inline split (delimiter : char) (text : string) = text.Split [| delimiter |] |> Array.toList

/// Splits the given string at the given string delimiter
let inline splitStr (delimiterStr : string) (text : string) = 
    text.Split([| delimiterStr |], StringSplitOptions.None) |> Array.toList

/// Converts a sequence of strings into a string separated with line ends
let inline toLines text = separated Environment.NewLine text

/// Checks whether the given text starts with the given prefix
let startsWith prefix (text : string) = text.StartsWith prefix

/// Checks whether the given text ends with the given suffix
let endsWith suffix (text : string) = text.EndsWith suffix

/// Determines whether the last character of the given <see cref="string" />
/// matches Path.DirectorySeparatorChar.         
let endsWithSlash = endsWith (Path.DirectorySeparatorChar.ToString())

/// Replaces the first occurrence of the pattern with the given replacement.
let replaceFirst (pattern : string) replacement (text : string) = 
    let pos = text.IndexOf pattern
    if pos < 0 then text
    else text.Remove(pos, pattern.Length).Insert(pos, replacement)

/// Appends a text to a StringBuilder.
let inline append text (builder : StringBuilder) = builder.Append(sprintf "\"%s\" " text)

/// Appends a text to a StringBuilder without surrounding quotes.
let inline appendWithoutQuotes (text : string) (builder : StringBuilder) = builder.Append(sprintf "%s " text)

/// Appends string of function value if option has some value
let inline appendIfSome o f builder = 
    match o with
    | Some(value) -> appendWithoutQuotes (f value) builder
    | None -> builder

/// Appends a text if the predicate is true.
let inline appendIfTrue p s builder = 
    if p then append s builder
    else builder

let inline appendIfTrueWithoutQuotes p s builder = 
    if p then appendWithoutQuotes s builder
    else builder

/// Appends a text if the predicate is false.
let inline appendIfFalse p = appendIfTrue (not p)

/// Appends a text without quoting if the value is not null.
let inline appendWithoutQuotesIfNotNull (value : Object) s = 
    appendIfTrueWithoutQuotes 
        (value <> null) 
        (match value with
         | :? String as sv -> (sprintf "%s%s" s sv)
         | _ -> (sprintf "%s%A" s value))

/// Appends a text if the value is not null.
let inline appendIfNotNull (value : Object) s = 
    appendIfTrue 
        (value <> null) 
        (match value with
         | :? String as sv -> (sprintf "%s%s" s sv)
         | _ -> (sprintf "%s%A" s value))

/// Appends a quoted text if the value is not null.
let inline appendQuotedIfNotNull (value : Object) s (builder : StringBuilder) = 
    if (value = null) then builder
    else 
        (match value with
         | :? String as sv -> builder.Append(sprintf "%s\"%s\" " s sv)
         | _ -> builder.Append(sprintf "%s\"%A\" " s value))

/// Appends a text if the value is not null.
let inline appendStringIfValueIsNotNull value = appendIfTrue (value <> null)

/// Appends a text if the value is not null or empty.
let inline appendStringIfValueIsNotNullOrEmpty value = appendIfTrue (isNullOrEmpty value |> not)

/// Appends a text if the value is not null or empty.
let inline appendIfNotNullOrEmpty value s = appendIfTrue (isNotNullOrEmpty value) (sprintf "%s%s" s value)

/// Appends all notnull fileNames.
let inline appendFileNamesIfNotNull fileNames (builder : StringBuilder) = 
    fileNames |> Seq.fold (fun builder file -> appendIfTrue (isNullOrEmpty file |> not) file builder) builder

/// Returns the text from the StringBuilder
let inline toText (builder : StringBuilder) = builder.ToString()

/// [omit]
let private regexes = new Dictionary<_, _>()

/// [omit]
let getRegEx pattern = 
    match regexes.TryGetValue pattern with
    | true, regex -> regex
    | _ -> (new System.Text.RegularExpressions.Regex(pattern))

/// [omit]
let regex_replace pattern (replacement : string) text = (getRegEx pattern).Replace(text, replacement)

/// Checks whether the given char is a german umlaut.
let isUmlaut c = Seq.exists ((=) c) [ 'ä'; 'ö'; 'ü'; 'Ä'; 'Ö'; 'Ü'; 'ß' ]

/// Converts all characters in a string to lower case.
let inline toLower (s : string) = s.ToLower()

/// Returns all standard chars and digits.
let charsAndDigits = [ 'a'..'z' ] @ [ 'A'..'Z' ] @ [ '0'..'9' ]

/// Checks whether the given char is a standard char or digit.
let isLetterOrDigit c = List.exists ((=) c) charsAndDigits

/// Trims the given string with the DirectorySeparatorChar
let inline trimSeparator (s : string) = s.TrimEnd Path.DirectorySeparatorChar

/// Trims all special characters from a string.
let inline trimSpecialChars (text : string) = 
    text
    |> Seq.filter isLetterOrDigit
    |> Seq.filter (isUmlaut >> not)
    |> Seq.fold (fun (acc : string) c -> acc + string c) ""

/// Trims the given string
let inline trim (x : string) = 
    if isNullOrEmpty x then x
    else x.Trim()

/// Trims the given string
let inline trimChars chars (x : string) = 
    if isNullOrEmpty x then x
    else x.Trim chars

/// Trims the start of the given string
let inline trimStartChars chars (x : string) =
    if isNullOrEmpty x then x
    else x.TrimStart chars

/// Trims the end of the given string
let inline trimEndChars chars (x : string) =
    if isNullOrEmpty x then x
    else x.TrimEnd chars

/// Lifts a string to an option
let liftString x = 
    if isNullOrEmpty x then None
    else Some x
