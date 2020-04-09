import pexpect
import sys
from subprocess import Popen, PIPE, STDOUT
import time
from sys import stdin

REPL_PS = str('\*HW2> ')
LOOPS = 100
input_file= open("test_input.txt","r")
test_out=open("test_output.txt","r")
output_file=open("output_file.txt","w+")
a_txt=open("a.txt","w+")
repl = pexpect.spawnu('ghci HW2.hs')
def proces_input(repl,input_file,output_file,test_out):
    repl.expect(REPL_PS)
    inp_lines = input_file.readlines()
    out_lines =[]
    for inp in inp_lines:
        if(inp=="end" or inp=="end\n"):
            return
        out=test_out.readline()
        repl.sendline("("+inp[:-1]+")"+"=="+"("+out[:-1]+")\n")
        _, haskell_output = repl.readline(), repl.readline()
        haskell_output=haskell_output[7:]
        #for unix
        #if(haskell_output!="True\n"):
        if(haskell_output!="True\r\n"):
            repl.expect(">")
            repl.expect(">")
            repl.sendline(inp)
            _, wrong_out = repl.readline(), repl.readline()
            print("input:"+inp[:]+"expected:"+out+"given:"+wrong_out)
            output_file.write("input:"+inp[:]+"expected:"+out+"given:"+wrong_out)
        repl.expect(">")
        repl.expect(">")
        inp=input_file.readline()
    repl.sendline(":q")


def generate_output(repl,input_file,output_file):
    repl.expect(REPL_PS)
    inp_lines = input_file.readlines()
    out_lines =[]
    for inp in inp_lines:
        if(inp=="end"):
            break
        out=test_out.readline()
        repl.sendline(inp)
        _, haskell_output = repl.readline(), repl.readline()
        haskell_output=haskell_output[7:]
        #for unix
        #if(haskell_output!="True\n"):

        output_file.write(haskell_output)
        repl.expect(">")
        repl.expect(">")
        inp=input_file.readline()
    output_file.write("end\n")
proces_input(repl,input_file,output_file,test_out)
input_file.close()
input_file= open("test_input.txt","r")
repl = pexpect.spawnu('ghci HW2.hs')
generate_output(repl,input_file,a_txt)
