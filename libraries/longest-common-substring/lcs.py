import sys

def lcs(S,T):

    m = len(S)
    n = len(T)
    counter = [[0]*(n+1) for x in range(m+1)]
    longest = 0
    lcs_set = set()
    for i in range(m):
        for j in range(n):
            if S[i] == T[j]:
                c = counter[i][j] + 1
                counter[i+1][j+1] = c
                if c > longest:
                    lcs_set = set()
                    longest = c
                    lcs_set.add(S[i-c+1:i+1])
                elif c == longest:
                    lcs_set.add(S[i-c+1:i+1])

    return lcs_set


def build_arg_parser():
    """
    Build an argument parser using argparse. Use it when python version is 2.7 or later.

    """
    parser = argparse.ArgumentParser(description="Longest Common Substring calculator -- arguments")
    parser.add_argument('-s', nargs=2, required=True, type=str,
                        help='Two strings to compare: expected sentence and generated sentence. This option is required.')
    return parser


def build_arg_parser2():
    """
    Build an argument parser using optparse. Use it when python version is 2.5 or 2.6.

    """
    usage_str = "Longest Common Substring calculator -- arguments"
    parser = optparse.OptionParser(usage=usage_str)
    parser.add_option("-s", "--sentences", nargs=2, dest="s", type="string",
                      help='Two strings to compare: expected sentence and generated sentence. This option is required.')
    return parser

if __name__ == "__main__":

    # use optparse if python version is 2.5 or 2.6
    if sys.version_info[1] < 7:
        import optparse
        if len(sys.argv) == 1:
            print >> ERROR_LOG, "No argument given. Please run smatch.py -h \
            to see the argument description."
            exit(1)
        parser = build_arg_parser2()
        (args, opts) = parser.parse_args()
        if args.s is None:
            print >> ERROR_LOG, "lcs.py requires -s option to indicate two sentences to compare. \
                                 Please run lcs.py -h to  \
                                 see the argument description."
            exit(1)
        # assert there are 2 network meanings following -f.
        assert(len(args.s) == 2)
    #  use argparse if python version is 2.7 or later
    else:
        import argparse
        parser = build_arg_parser()
        args = parser.parse_args()

    ret = lcs(args.s[0].lower(), args.s[1].lower())
    # look only at the first sentence. If everything went well then possible there is only one sequence
    words = ret.pop().strip().split(' ')
    initial_words = args.s[0].lower().strip().split(' ')
    # remove incomplete words
    count = 0
    sentence = ''
    for w in words:
	for w1 in initial_words:
       	    if w == w1 :
	       sentence += w
	       sentence += ' '
	       count += 1       

print str(count)

