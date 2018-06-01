def quote(input)
    result = ""
    for i in 0...input.length
        c = input[i]
        case c
        when "\n" then
            result += "\" NEWLINE \""
        when '"' then
            result += "\" QUOTE \""
        when ':' then
            result += "\" ESCAPE \""
        else
            result += c
        end
    end
    result
end
def quote_yarn(input)
    result = ""
    for i in 0...input.length
        c = input[i]
        case c
        when "\n" then
            result += ":)"
        when '"' then
            result += ":\""
        when ':' then
            result += "::"
        else
            result += c
        end
    end
    result
end

footer = 'IF U SAY SO

VISIBLE I IZ HEADER YR ":)" AN YR ":"" AN YR "::" MKAY !
VISIBLE ":"" !
VISIBLE I IZ HEADER YR ":" NEWLINE :"" AN YR ":" QUOTE :"" AN YR ":" ESCAPE :"" MKAY !
VISIBLE ":""
VISIBLE FOOTER

KTHXBYE'
header = 'HAI 1.2

I HAS A FOOTER ITZ "' + quote_yarn(footer) + '"
HOW IZ I HEADER YR NEWLINE AN YR QUOTE AN YR ESCAPE
    SMOOSH '

print header
print '"'
print quote(header)
puts '"'
puts footer
