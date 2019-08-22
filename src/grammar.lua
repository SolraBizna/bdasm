globals("grammar")

local lpeg = require "lpeg"

local P = lpeg.P
local V = lpeg.V
local C = lpeg.C
local R = lpeg.R
local S = lpeg.S
local Ct = lpeg.Ct
local Cc = lpeg.Cc

local ESCAPES = {
   ["\\"] = "\\",
   a="\x07",
   n="\n",
   t="\t",
   r="\r",
}

grammar = P{
   "loc";
   loc = V"label" * (V"instruction" + V"eol");
   label = (-1 * Cc"") + (C(V"identifier") * (P":")^-1 + Cc"") * (V"whitespace"^1+V"eol");
   eol = V"whitespace"^0 * (P";" + -1);
   identifier = (R("az","AZ")+S"_") * (R("az","AZ","09")+S"_")^0 + P"-"^1 + P"+"^1;
   instruction = (Cc"instruction"*(C(V"identifier")/string.upper)*((P"."*C(V"identifier")/string.upper)+C"")*(P"?"*Cc(true)+Cc(false))+(Cc"directive"*P"."*(C(V"identifier")/string.upper)*Cc""*Cc(false))) * Ct(V"whitespace"^1 * V"nonterminal_parameter"^0 * V"parameter" + V"eol");
   nonterminal_parameter = V"parameter" * V"whitespace"^0 * P"," * V"whitespace"^0;
   parameter = V"param_string" + V"param_id" + V"param_literal";
   param_string = V"squot_string" + V"dquot_string";
   squot_string = Ct(Cc"string"*P"'"*C((P(1)-P"'")^0)*"'");
   dquot_string = Ct(Cc"string"*P'"'*(Ct((V"dquot_string_char"-P'"')^0)/table.concat)*P'"');
   dquot_string_char = V"dquot_escaped_char" + C(1);
   dquot_escaped_char = P"\\" * (C(S"\\antr") / ESCAPES);
   param_id = Ct(Cc"identifier"*C(V"identifier"));
   param_literal = Ct(Cc"integer"*(V"num_hex" + V"num_bin" + V"num_dec"));
   num_hex = P"0x" * C(R("09","AF","af")^1)/function(x) return tonumber(x,16) end;
   num_bin = P"%" * C(S"01"^1)/function(x) return tonumber(x,2) end;
   num_dec = C(P"-"^-1*R("09")^1)/function(x) return tonumber(x,10) end;
   whitespace = S" \r\t";
}
