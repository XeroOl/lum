local lpeg = require "lpeglabel"
local _G = _G
local lum = {}

lum.version = "2.0.0"

setfenv(1, setmetatable({}, {
	__index = function(self, k)
		return _G[k] or lpeg[k]
	end
}))

local eof = P "" - 1
local comment = P "/*" * ((C '\n' + 1) - P "*/") ^ 0 * P "*/" + P "//" * (1 - P "\n") ^ 0 * (C "\n" + eof)
local Space = P{(P " " + comment + C"\t" + C"\n") ^ 1 * -V(1)}

local function maybe(obj)
	return P(obj) ^ -1
end
local function maybe_some(obj)
	return P(obj) ^ 0
end
local function some(obj)
	return P(obj) ^ 1
end
local function token(str)
	return Space ^ -1 * C(str)
end
local function delete(patt)
	return Space ^ -1 * (P(patt) / 0)
end
local swap_ba
do
	local function upvalue(a, b)
		local lena = #a
		local lenb = #b
		for i = 1, lena do
			b[i + lenb] = a[i]
		end
		return unpack(b)
	end
	function swap_ba(patt_a, patt_b)
		return Ct(patt_a) * Ct(patt_b) / upvalue
	end
end
local swap_aba
do
	local function upvalue(a, b)
		local lenb = #b
		local lena = #a
		for i = 1, lenb do
			a[#a + 1] = b[i]
		end
		for i = 1, lena do
			if a[i] ~= '\n' then
				a[#a + 1] = a[i]
			end
		end
		return unpack(a)
	end
	function swap_aba(patt_a, patt_b)
		return Ct(patt_a) * Ct(patt_b) / upvalue
	end
end
local insert = Cc
local t, d, i = token, delete, insert

local keywords = {
	['and'] = true, ['break'] = true, ['do'] = true, ['else'] = true,
	['elseif'] = true, ['end'] = true, ['false'] = true, ['for'] = true,
	['function'] = true, ['if'] = true, ['in'] = true, ['local'] = true,
	['nil'] = true, ['not'] = true, ['or'] = true, ['repeat'] = true,
	['return'] = true, ['then'] = true, ['true'] = true, ['until'] = true,
	['while'] = true, ['fn'] = true, ['pub'] = true, ['let'] = true,
}
local letter = R 'az' + R 'AZ' + '_'
local digit = R '09'
local Name = token(Cmt(C(letter * maybe_some(letter + digit)),
	function(s, i, a)
		if not keywords[a] then
			return i
		end
	end
))

local equals = maybe_some "="
local open = "[" * Cg(equals, "init") * "["
local close = "]" * C(equals) * "]"
local closeeq = lpeg.Cmt(close * lpeg.Cb("init"), function (s, i, a, b) return a == b end)
local String = token (
	open * maybe_some(1 - closeeq) * close / 0
	+ "'" * maybe_some("\\'" + (1 - S "'\n")) * "'"
	+ '"' * maybe_some('\\"' + (1 - S '"\n')) * '"'
)

local nonzero_digit = R '19'
local hex_digit = R '09' + R 'af'
local zero = P  '0'
local Number = token (
	R '19' * maybe_some(R '09') * maybe('.' * maybe_some(R '09')) * maybe('e' * some(R '09'))
	+ '0' * maybe('.' * maybe_some(R '09')) * maybe('e' * some(R '09'))
	+ '.' * some(R '09') * maybe('e' * maybe_some(R '09'))
	+ '0x.' * some(hex_digit) * maybe('e' * some(R '09'))
	+ '0x' * some(hex_digit) * maybe('.' * maybe_some(hex_digit)) * maybe('e' * some(R '09'))
)

local chunk = V "chunk"
local block = V "block"
local stat = V "stat"
local laststat = V "laststat"
local funcname = V "funcname"
local varlist = V "varlist"
local var = V "var"
local namelist = V "namelist"
local explist = V "explist"
local exp = V "exp"
local prefixexp = V "prefixexp"
local functioncall = V "functioncall"
local args = V "args"
local _function = V "_function"
local funcbody = V "funcbody"
local parlist = V "parlist"
local tableconstructor = V "tableconstructor"
local fieldlist = V "fieldlist"
local field  = V "field"
local fieldsep = V "fieldsep"
local binop = V "binop"
local unop = V "unop"

-- new things
local exp2 = V "exp2"
local call = V "call"
local index = V "index"
local prefix = V "prefix"
local bracedblock = V "bracedblock"
local semistat = V "semistat"
local functioncall2 = V "functioncall2"
local prefixexp2 = V 'prefixexp2'
local var2 = V "var2"
local file = V "file"
local preindex = V "preindex"
local preindex2 = V "preindex2"

------------- STOLEN FROM http://lua-users.org/wiki/LpegRecipes ON Jan 20, 2020 --------------
-- Let's come up with a syntax that does not use left recursion
-- (only listing changes to Lua 5.1 extended BNF syntax)
-- prefix ::= '(' exp ')' | Name
-- index ::= '[' exp ']' | '.' Name
-- call ::= args | ':' Name args
-- suffix ::= call | index
-- var ::= prefix {suffix} index | Name
-- functioncall ::= prefix {suffix} call
-----------------------------------------------------------------------------------------------

local compiler = Ct {
	file,
	file = chunk * maybe(Space) * eof,
	chunk = stat ^ 0 * laststat ^ -1,
	bracedblock = d '{' * i 'do' * block * d '}' * i 'end',
	block = chunk,
	stat 
		= bracedblock
		+ t 'while' * d '(' * exp * d ')' * bracedblock
		+ d 'do' * d '{' * i 'repeat' * block * d '}' * d 'while' * i 'until' * i 'not' * t '(' * exp * t ')' * t ';'
		+ t 'if' * d '(' * exp * d ')' * d '{' * i 'then' * block
			* maybe_some(d '}' * d 'else' * d 'if' * i 'elseif' * d '(' * exp * d ')' * d '{' * i 'then' * block)
			* maybe(d '}' * t 'else' * d '{' * block) * d '}' * i 'end'
		+ t 'for' * d '(' * Name * t '=' * exp * t ',' * exp * maybe(t ',' * exp) * d ')' * bracedblock
		+ t 'for' * d '(' * namelist * d ':' * i 'in' * explist * d ')' * bracedblock
		+ d 'pub' * d 'fn' * i 'function' * funcname * funcbody
		+ d 'fn' * i 'local' * i 'function' * Name * funcbody
		+ d 'fn' * i 'function' * funcname * funcbody
		+ d 'loop' * i 'while' * i 'true' * bracedblock
		+ d 'for' * d '(' * i 'do' * maybe(semistat) * t ';' * i 'while' * (exp + Cc 'true') * d ';'
			* swap_ba(
				semistat * i ';' * i 'end',
				d ')' * i 'do' * bracedblock
			) * i 'end'
		+ semistat * t ';'
	,
	semistat
		= d 'let' * i 'local' * namelist * maybe(t '=' * explist)
		+ varlist * t '=' * explist
		-- TODO simplify
		+ swap_aba(Name * maybe(index), d '+=' * i '=') * i '+' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '-=' * i '=') * i '-' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '*=' * i '=') * i '*' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '/=' * i '=') * i '/' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '%=' * i '=') * i '%' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '^=' * i '=') * i '^' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '||=' * i '=') * i 'or' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '&&=' * i '=') * i 'and' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '..=' * i '=') * i '..' * (exp2 * -binop + i '(' * exp * i ')')
		+ swap_aba(Name * maybe(index), d '++' * i '=') * i '+' * i '1'
		+ swap_aba(Name * maybe(index), d '--' * i '=') * i '-' * i '1'
		+ i 'local' * i '_' * i '=' * preindex * i ';' * (
			-- TODO simplify
			swap_aba(i '_' * index, d '+=' * i '=') * i '+' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '-=' * i '=') * i '-' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '*=' * i '=') * i '*' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '/=' * i '=') * i '/' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '%=' * i '=') * i '%' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '^=' * i '=') * i '^' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '||=' * i '=') * i 'or' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '&&=' * i '=') * i 'and' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '..=' * i '=') * i '..' * (exp2 * -binop + i '(' * exp * i ')')
			+ swap_aba(i '_' * index, d '++' * i '=') * i '+' * i '1'
			+ swap_aba(i '_' * index, d '--' * i '=') * i '-' * i '1'
		)
		+ functioncall
	,
	laststat
		= t 'return' * maybe(explist) * t ';'
		+ t 'break' * ';'
	,
	preindex = prefix * preindex2 + Name,
	preindex2 = (call + index) * (#((index * ("" - index - call))/ 0) + preindex2),
	funcname = Name * maybe_some(t '.' * Name) * maybe(t ':' * Name),
	varlist = var * maybe_some(t ',' * var),
	var = prefix * var2 + Name + '_',
	var2 = index * maybe(var2) + call * var2,
	namelist = Name * maybe_some(t ',' * Name),
	explist = exp * maybe_some(t ',' * exp),
	exp = exp2 * maybe_some(binop * exp2),
	exp2
		= functioncall
		+ Number
		+ String
		+ _function
		+ t '...'
		+ prefixexp
		+ tableconstructor
		+ t 'nil'
		+ t 'false'
		+ t 'true'
		+ unop * exp2,
	prefixexp = prefix * maybe(prefixexp2),
	prefixexp2 = (call + index) * maybe(prefixexp2),
	functioncall = prefix * functioncall2,
	functioncall2 = call * maybe(functioncall2) + index * functioncall2,
	args = t '(' * maybe(explist) * t')' + tableconstructor + String,
	_function = i 'function' * (
			t '(' * maybe(parlist) * t ')'
			+ i '(' * (Name + t '...') * i ')'
	) *
		d '->'
	* (
		d '{' * block * d '}' 
		+ d '(' * i 'return' * maybe(explist) * d ')' * i ';'
		+ i 'return' * exp * i ';'
	) * i 'end',
	funcbody = t '(' * maybe(parlist) * t ')' * d '{' * block * d '}' * i 'end',
	parlist = namelist * maybe (t ',' * t '...') + t '...',
	tableconstructor = t '{' * maybe(fieldlist) * t '}',
	fieldlist = field * maybe(fieldsep * maybe(fieldlist)),
	field = t '[' * exp * t ']' * t '=' * exp + Name * t '=' * exp + exp,
	fieldsep = t ';' + t ',',
	binop = t(P'^' + P'<=' + '>=' + '==' + '..' + S'+-*/%<>^') + d '&&' * i 'and' + d '!=' * i '~=' + d '||' * i 'or',
	unop = t '-' + d '!' * i 'not' + t '#',
	call = args + t ':' * Name * args,
	index = t '[' * exp * t ']'  + d '.' * (i '.' * Name + i '[' * t(R'09'^1) * i ']'),
	prefix = t '(' * exp * t ')' + Name,
} / function(tab)
	return table.concat(tab, ' ')
		:gsub(" \n", "\n")
		:gsub("\n ", "\n")
		:gsub("\t ", "\t")
end

local function count_lines(str, pos)
	local result = Cf(Cc{1, pos} * ('\n' * Cp() + 1) ^ 0, function(a, b)
		a[1] = b <= pos and a[1] + 1 or a[1]
		a[2] = b <= pos and a[2] > pos - b + 1 and pos - b + 1 or a[2]
		return a
	end):match(str)
	return result[1], result[2]
end

local function make_err_message(str, pos)
	local row, col = count_lines(str, pos)
	return 'Syntax error on line ' .. row ..' at col ' .. col
end

function lum.to_lua(str, check)
	local processed, _, errpos = compiler:match(str)
	if not processed then
		return nil, make_err_message(str, errpos)
	end
	return processed, nil
end

--------------------------------------------------------------------------------
function lum.loadstring(str, ...)
	local lua_code, err = lum.to_lua(str)
	if not lua_code then
		return nil, err
	else
		return loadstring(lua_code, ...)
	end
end

lum.load = lum.loadstring

function lum.loadfile(path, ...)
	local file, err = io.open(path)
	if err then
		return nil, err
	end
	local block = assert(file:read("*a"))
	return lum.loadstring(block, ...)
end

function lum.dostring(str, ...)
	return assert(lum.loadstring(str, ...))()
end

function lum.dofile(path, ...)
	return assert(lum.loadfile(str, ...))()
end

local dirsep = '/'

function lum.create_lumpath(path)
	return path:gsub("%.lua", ".lum")
end

function lum.insert_loader(pos)
	pos = pos or 2
	if not package.lumpath then
		package.lumpath = lum.create_lumpath(package.path)
	end
	local loaders = package.loaders or package.searchers
	for _, v in ipairs(loaders) do
		if v == lum.lum_loader then
			return false
		end
	end
	table.insert(loaders, pos, lum.lum_loader)
	return true
end

function remove_loader()
	package.lumpath = nil
	local loaders = package.loaders or package.searchers
	for i, v in ipairs(loaders) do
		if v == lum.lum_loader then
			table.remove(loaders, i)
			return true
		end
	end
	return false
end

function lum.lum_loader(name)
	name_path = name:gsub("%.", dirsep)
	print(name)
	if package.lumpath then
		for path in package.lumpath:gmatch("[^;]+") do
			local path = path:gsub("?", name_path)
			local file = io.open(path)
			if file then
				local loaded, err = lum.loadstring(file:read("*a"))
				file:close();
				if err then
					error(path .. ": " .. err)
				end
				return loaded
			end
		end
	end
	return nil, "lum loader found no files"
end

lum.insert_loader()

return lum
