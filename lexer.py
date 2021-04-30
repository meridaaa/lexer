import ply.lex as lex
import nltk

CONST_SPECIAL_CHARACTERS = u'\xf1\xe1\xe9\xed\xf3\xfa\xc1\xc9\xcd\xd3\xda\xd1'
dato = ""
column = 0
error = 0
list = []
reserved = {
    'begin': 'TkBegin',
    'end': 'TkEnd',
    'using': 'TkUsing',
    'integer': 'TkInteger',
    'boolean': 'TkBoolean',
    'canvas': 'TkCanvas',
    'if': 'TkIf',
    'then': 'TkThen',
    'else': 'TkElse',
    'done': 'TkDone',
    'while': 'TkWhile',
    'repeat': 'TkRepeat',
    'true': 'TkTrue',
    'false': 'TkFalse',
    'with': 'TkWith',
    'from': 'TkFrom',
    'to': 'TkTo',
    'read' : 'TkRead',
    'print'	: 'TkPrint',

}

tokens = (
             'TkIdent',
             'TkOfType',
             'TkFloat',
             'NUMBER',
             'TkInt',
             'TkSci',
             'TkLienzo',
             'TkComa',
             'TkPuntoYComa',
             'TkParAbre',
             'TkParCierra',
             'TkSuma',
             'TkResta',
             'TkMult',
             'TkDiv',
             'TkMod',
             'TkConjuncion',
             'TkDisyuncion',
             'TkNegacion',
             'TkMenor',
             'TkMenorIgual',
             'TkMayor',
             'TkMayorIgual',
             'TkIgual',
             'TkDesIgual',
             'TkHorConcat',
             'TkVerConcat',
             'TkRot',
             'TkTras',
             'TkAsignacion',
             'TkLeftSQRBRACKET',
             'TkRightSQRBRACKET',
             'TkString',
             'TkNull'
         ) +tuple(reserved.values())

# Releftglas para tokens simples
t_TkLeftSQRBRACKET = r'\['
t_TkRightSQRBRACKET = r'\]'
t_TkComa = r'\,'
t_TkPuntoYComa = r'\;'
t_TkParAbre = r'\('
t_TkParCierra = r'\)'
t_TkSuma = r'\+'
t_TkResta = r'\-'
t_TkMult = r'\*'
t_TkDiv = r'\/'
t_TkMod = r'\%'
t_TkConjuncion = r'\/\\'
t_TkDisyuncion = r'\\\/'
t_TkNegacion = r'\^'
t_TkMenor = r'\<'
t_TkMenorIgual = r'\<='
t_TkMayor = r'>'
t_TkMayorIgual = r'>='
t_TkIgual = r'\='
t_TkDesIgual = r'\/='
t_TkHorConcat = r'\:'
t_TkVerConcat = r'\|'
t_TkRot = r'\$'
t_TkTras = r'\''
t_TkAsignacion = r':='
t_ignore  = " \t"



def t_TkOfType(t):
    r'of\s+type'
    t.value = 'of type'
    return t


def t_STRING(t):
    r'(\'[a-zA-Z_]\w*\')| (\"[a-zA-Z_]\w*\")'
    t.type = reserved.get(t.value, 'TkString')
    return t

def t_TkNull(t):
    r'null'
    t.value = None
    return t

def t_TkFloat(t):
    r"((\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?[ij]?)"
    if (t.value + 'e'):
        t.value = t.value + 'e'
        t.type = reserved.get(t.value, 'TkSci')
    elif ('.' not in t.value):
        t.value = int(t.value)
        t.type = reserved.get(t.value, 'TkInt')
    elif(t.value + 'e'):
        print('d')
    else:
        t.value = float(t.value)
    return t





symbol_table=[]
def t_TkIdent(t):
        r'[a-zA-Z_]\w*'
        if(t.type != 'TkSci'):
         t.type = reserved.get(t.value, 'TkIdent')
         if(t.value in r'[a-zA-Z_]\w*'):
          is_symolTable=''
          if not(t.value in symbol_table):
           symbol_table.append(t.value)
           is_true = 'true'
           is_symolTable = is_true +' '+ t.value
          else:
           is_true ='false'
           is_symolTable = is_true + ' ' + t.value
          print(is_symolTable)
        return t








def t_TkLienzo(t):
    r'((</>)|(<->)|(<_>)|(<empty>)|(<\\>)|(<\|>)|(< >))'
    t.type = reserved.get(t.value,' TkLienzo')
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def find_column(input,token):
    last_cr = input.rfind('\n',0 ,token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column


#  errors##
def t_error(t):
    global dato
    column = find_column(dato,t )
    if (t.lexer.lineno==1 ):
        print
        "Error: Unexpected charecter '%s'" % t.value[0],"  in the fila ", t.lexer.lineno," , column ", column

    else:
        print
        "Error: Unexpected charecter '%s'" % t.value[0],"  in the fila ", t.lexer.lineno," , column ", column- 1

    global error
    error= 1
    print('ERROR')
    t.lexer.skip(1)


def accentReplace(word):
    identifier = ''
    for i in word:
        index = word.index(i)
        if i in CONST_SPECIAL_CHARACTERS:
            identifier += '_'
        else:
            identifier += i
    return identifier

def t_comment(t):
    r'(((\{\-)+)[^-]*(\-(?!\{))*)[^{-]*(\-(?!\{))*(\-\}){1}'
    pass

lexer = lex.lex()

data = ""
while 1:
    try:
        data = data + input() + " \n"
    except EOFError:
        break

lexer.input(data)

while True:
    tok = lexer.token()
    if not tok: break  # No more input
    if (error==0 ):
        list.append((tok.type,tok.value))
    else:
        pass
if(('TkSci', '2e') in list):
    list.remove(('TkIdent', 'e'))
if (error==0 ):
    print (list)
