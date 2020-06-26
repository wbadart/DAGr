try:
    from os import environ
    i1 = environ['FOO']
    printer = print(i1)
finally:
    del environ['FOO']
