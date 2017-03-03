import re
def optimize(brainfuck):
    stupidPatterns = ["<>","><","\+-","-\+"]
    for bad in stupidPatterns:
        while(re.search(bad,brainfuck)):
            brainfuck = re.sub(bad,'',brainfuck)
    return brainfuck

