{
module F = Format

let rec pp ppf = function
  | `String s ->
      F.fprintf ppf "%S" s
  | `Object l ->
      let pp_field ppf (s, v) = F.fprintf ppf "@[<2>%S: %a@]" s pp v in
      F.fprintf ppf "@[<v>{@;<0 2>@[%a@]@;<0 -2>}@]"
        (F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf ",@ ") pp_field) l
  | `Array l ->
      F.fprintf ppf "@[<v>[%a]@]"
        (F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf ",@ ") pp) l
  | `True ->
      F.pp_print_string ppf "true"
  | `False ->
      F.pp_print_string ppf "false"
  | `Null ->
      F.pp_print_string ppf "null"
  | `Number s ->
      F.pp_print_float ppf (float_of_string s)
}

let ws = ['\x20''\x0A''\x0D''\x09']*
let digit = ['0'-'9']
let digits = digit+
let onenine = ['1'-'9']
let integer = digit | onenine digits | '-' digit | '-' onenine digits
let fraction = ('.' digits)?
let sign = ['-''+']?
let exponent = (['e''E'] sign digits)?
let number = integer fraction exponent

rule value = parse
| ws '"' { `String (str (Buffer.create 101) lexbuf) }
| ws '{' ws '}' ws { `Object [] }
| ws '{' { let v = field lexbuf in obj [v] lexbuf }
| ws '[' ws ']' ws { `Array [] }
| ws '[' { let v = value lexbuf in arr [v] lexbuf }
| ws "true" ws { `True }
| ws "false" ws { `False }
| ws "null" ws { `Null }
| ws (number as s) ws { `Number s }

and str buf = parse
| '"' ws { Buffer.contents buf }
| _ as c { Buffer.add_char buf c; str buf lexbuf }

and obj accu = parse
| '}' ws { `Object (List.rev accu) }
| ',' { let v = field lexbuf in obj (v :: accu) lexbuf }

and field = parse
| ws '"' { let s = str (Buffer.create 101) lexbuf in field1 s lexbuf }

and field1 s = parse
| ws ':' ws { let v = value lexbuf in s, v }

and arr accu = parse
| ']' ws { `Array (List.rev accu) }
| ',' { let v = value lexbuf in arr (v :: accu) lexbuf }
