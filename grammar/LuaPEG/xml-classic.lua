
local lpeg = require "lpeg"
local File = lpeg.V"File"
local Xml = lpeg.V"Xml"
local NAME = lpeg.V"NAME"
local CDATA = lpeg.V"CDATA"
G = lpeg.P{ File,
      File = (lpeg.P"<"  *  ( lpeg.P"?"  *  ( lpeg.P"x"  *  ( lpeg.P"m"  *  ( lpeg.P"l"  *  ( (-(lpeg.P'?>') *  lpeg.P(1))^0 *  ( lpeg.P"?"  *  ( lpeg.P">"  *  (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 ) ) ) ) ) ) ))^-1 *  ( (lpeg.P"<"  *  ( lpeg.P"!"  *  ( (-(lpeg.P">" ) *  lpeg.P(1))^0 *  ( lpeg.P">"  *  (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 ) ) ))^-1 *  Xml  );
      Xml = lpeg.P"<"  *  ( NAME  *  ( (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 *  ( (NAME  *  ( (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 *  ( lpeg.P"="  *  ( (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 *  ( ( lpeg.P"\""  *  ( (-(lpeg.P"\"" ) *  lpeg.P(1))^0 *  lpeg.P"\""  ) ) *  (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 ) ) ) ))^0 *  ( (  ( lpeg.P'/>' )  +  ( lpeg.P">"  *  ( (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 *  ( ( ( Xml  )  +  ( lpeg.P"<"  *  ( lpeg.P"!"  *  ( lpeg.P"["  *  ( lpeg.P"C"  *  ( lpeg.P"D"  *  ( lpeg.P"A"  *  ( lpeg.P"T"  *  ( lpeg.P"A"  *  ( lpeg.P"["  *  ( CDATA  *  ( lpeg.P"]"  *  ( lpeg.P"]"  *  ( lpeg.P">"  *  (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 ) ) ) ) ) ) ) ) ) ) ) ) )  +  ( (-(lpeg.P"<" ) *  lpeg.P(1))^1 )  +  ( lpeg.P"<"  *  ( lpeg.P"!"  *  ( lpeg.P"-"  *  ( lpeg.P"-"  *  ( (-(lpeg.P'-->') *  lpeg.P(1))^0 *  ( lpeg.P"-"  *  ( lpeg.P"-"  *  ( lpeg.P">"  *  (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 ) ) ) ) ) ) ) ) )^0 *  ( lpeg.P"<"  *  ( lpeg.P"/"  *  ( NAME  *  lpeg.P">"  ) ) ) ) ) )  ) *  (lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " )^0 ) ) ) );
      NAME = lpeg.P":" + lpeg.R("AZ") + lpeg.P"_" + lpeg.R("az")  *  (lpeg.R("-.") + lpeg.R("0:") + lpeg.R("AZ") + lpeg.P"_" + lpeg.R("az") )^0;
      CDATA = (-(lpeg.P']]>') *  ( -(lpeg.P'<![CDATA[') *  lpeg.P(1) ))^0 *  (lpeg.P"<"  *  ( lpeg.P"!"  *  ( lpeg.P"["  *  ( lpeg.P"C"  *  ( lpeg.P"D"  *  ( lpeg.P"A"  *  ( lpeg.P"T"  *  ( lpeg.P"A"  *  ( lpeg.P"["  *  ( CDATA  *  ( lpeg.P"]"  *  ( lpeg.P"]"  *  ( lpeg.P">"  *  CDATA  ) ) ) ) ) ) ) ) ) ) ) ))^-1;
}

function evalExp (s)
   latency = -1.0
   for i = 0, 5 do
      local t1 = os.clock()
      local t = lpeg.match(G, s)
      local e1 = os.clock() - t1
      -- print("elapsedTime: ", e1)
      if latency == -1.0 or latency > e1 then
        latency = e1
      end
      if not t then return -1 end
      if (string.len(s) > t) then return -2 end
   end
   return (latency * 1000.0)
end

fileName = arg[1]
fh, msg = io.open(fileName, "r")
if fh then
   data = fh:read("*a")
else
   print(msg)
end
latency = evalExp(data)
if latency == -1 then
  print(fileName..", syntax error")
elseif latency == -2 then
  print(fileName..", unconsume")
else
  print(fileName..", "..latency)
end
