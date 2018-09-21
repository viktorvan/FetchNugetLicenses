#I @"packages"
#r @"FSharp.Data/lib/net45/FSharp.Data.dll"
#r @"System.Xml.Linq.dll"
#load "ListExtensions.fs"
open FSharp.Data
open System.IO
open Extensions

type PackagesConfig = XmlProvider<"./PackagesExample.xml">
type NugetMetadata = JsonProvider<"./NugetMetadata.json">

type LicenseUrl =
    | Url of string
    | Missing
    | Error of string
type LicenseInfo =
    { Name: string
    ; Url: LicenseUrl
    }

type LicenseType =
    | MIT
    | Apache
    | GPL_2
    | GPL_3
    | BSD_3
    | LGPL_2
    | Unknown

let (|MIT|_|) (text:string) =
    match text.ToLower().Contains("expat") with
    | true -> Some MIT
    | false -> None

let (|Apache|_|) (text:string) =
    match text.ToLower().Contains("apache") with
    | true -> Some Apache
    | false -> None

let (|GPL_2|_|) (text:string) =
    match text.ToLower().Contains("gpl-2") with
    | true -> Some GPL_2
    | false -> None

let (|GPL_3|_|) (text:string) =
    match text.ToLower().Contains("gpl-3") with
    | true -> Some GPL_3
    | false -> None

let (|BSD_3|_|) (text:string) =
    match text.ToLower().Contains("bsd") with
    | true -> Some BSD_3
    | false -> None

let (|LGPL_2|_|) (text:string) =
    match text.ToLower().Contains("lgpl") with
    | true -> Some LGPL_2
    | false -> None

type LicenseData =
    { Name: string
    ; Url: LicenseUrl
    ; LicenseType: LicenseType
    }

let rec getAllPackageConfigs dir = 
    let all = 
        seq { 
            yield! Directory.EnumerateFiles(dir, "packages.config")
            for d in Directory.EnumerateDirectories(dir) do
                yield! getAllPackageConfigs d
        } |> Seq.toList
    all

let parsePackageNames file =
    printf "parsing package name from %s\n" file
    PackagesConfig.Load(uri = file)
    |> (fun p -> p.Packages)
    |> Array.map (fun p -> p.Id)
    |> Array.toList

let getErrorMsg ex = 
    match (ex:exn) with
    | :? System.Net.WebException as ex ->
        match ex.Response with
        | :? System.Net.HttpWebResponse as res -> res.StatusCode |> int |> string 
        | _ -> "WebException" 
    | _ -> "Exception"

let getPackageMetadata (pid:string) =
    let url = sprintf "https://api.nuget.org/v3/registration3/%s/index.json" (pid.ToLower())
    async {
        let! metadata = NugetMetadata.AsyncLoad(url) |> Async.Catch
        match metadata with
        | Choice1Of2 m ->
            let licenseUrl = 
                m.Items 
                |> Array.tryLast
                |> Option.map (fun x -> x.Items)
                |> Option.map Array.tryLast
                |> Option.flatten
                |> Option.map (fun x -> x.CatalogEntry.LicenseUrl)

            printf "."

            match licenseUrl with
            | Some u ->
                return { LicenseInfo.Name = pid; Url = Url u }
            | None -> return { LicenseInfo.Name = pid; Url = Missing }
        | Choice2Of2 error -> 
            let msg = getErrorMsg error
            return { LicenseInfo.Name = pid; Url = Error msg }
    }

let getLicense { LicenseInfo.Name = name; Url = url } =
    let parseLicense = function
        | MIT -> MIT
        | Apache -> Apache
        | GPL_2 -> GPL_2
        | GPL_3 -> GPL_3
        | BSD_3 -> BSD_3
        | LGPL_2 -> LGPL_2
        | _ -> Unknown
    
    async {
        match url with
        | Url u when System.String.IsNullOrWhiteSpace(u) |> not ->
            let! text = Http.AsyncRequestString(u) |> Async.Catch 
            printf "."
            match text with
            | Choice1Of2 t ->
                return { Name = name; Url = Url u; LicenseType = parseLicense t}
            | Choice2Of2 error -> 
                let msg = getErrorMsg error
                return { Name = name; Url = Error msg; LicenseType = Unknown }
        | Url _ | Missing -> return { Name = name; Url = Missing; LicenseType = Unknown }
        | Error msg -> return { Name = name; Url = Error msg; LicenseType = Unknown }
    }

let writeToFile (data: LicenseData list) =
    let parseDataRow r =
        let url =
            match r.Url with
            | Url u -> u
            | Missing -> "-"
            | Error msg -> msg
        sprintf "%s; %s; %A" r.Name url r.LicenseType

    let now = System.DateTime.Now
    let filename = sprintf "./licenses-%s.csv" (now.ToString("yyyyMMdd-HHmmss")) 
    printf "\nWriting results to %s\n" filename
    File.WriteAllLines(filename, List.map parseDataRow data)
    printf "Done!\n"

let parseDir = function
    | [] -> failwith "Argument missing"
    | [script; dir ] -> dir
    | _ -> failwith "Too many arguments"

let dir = Array.toList fsi.CommandLineArgs |> parseDir

let printNumberOfUniquePackages list =
    list
    |> List.length
    |> printf "\nFetching nuget metadata for %i unique packages\n"
    |> ignore
    for i = 1 to List.length list do
        printf "."
    printf "\n"
    list

let printNumberOfFoundUrls (arr:LicenseInfo []) =
    let count =
        arr 
        |> Array.map (fun (x:LicenseInfo) -> match x.Url with | Url u -> Some u | _ -> None)
        |> Array.choose id 
        |> Array.map (fun x -> if System.String.IsNullOrWhiteSpace(x) then None else Some x)
        |> Array.choose id 
        |> Array.length
    count
    |> printf "\n\nFound %i license urls\n"
    |> ignore
    for i = 1 to count do
        printf "."
    printf "\n"
    arr

getAllPackageConfigs dir
    |> List.map parsePackageNames 
    |> List.concat
    |> List.distinct
    |> printNumberOfUniquePackages
    |> List.map getPackageMetadata |> Async.Parallel |> Async.RunSynchronously
    |> printNumberOfFoundUrls
    |> Array.map getLicense |> Async.Parallel |> Async.RunSynchronously
    |> Array.toList
    |> List.sortBy (fun x -> x.Name)
    |> writeToFile
