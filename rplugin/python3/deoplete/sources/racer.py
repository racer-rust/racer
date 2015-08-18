#=============================================================================
# FILE: racer.py
# AUTHOR:  Shougo Matsushita <Shougo.Matsu at gmail.com>
# License: MIT license  {{{
#     Permission is hereby granted, free of charge, to any person obtaining
#     a copy of this software and associated documentation files (the
#     "Software"), to deal in the Software without restriction, including
#     without limitation the rights to use, copy, modify, merge, publish,
#     distribute, sublicense, and/or sell copies of the Software, and to
#     permit persons to whom the Software is furnished to do so, subject to
#     the following conditions:
#
#     The above copyright notice and this permission notice shall be included
#     in all copies or substantial portions of the Software.
#
#     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
#     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
#     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
#     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# }}}
#=============================================================================

import re
import os
import subprocess
import tempfile
from .base import Base

class Source(Base):
    def __init__(self, vim):
        Base.__init__(self, vim)

        self.name = 'racer'
        self.mark = '[racer]'
        self.filetypes = ['rust']
        self.min_pattern_length = 0
        self.executable_racer = self.vim.eval('executable(g:racer_cmd)')
        self.racer = self.vim.eval('g:racer_cmd')
        self.encoding = self.vim.eval('&encoding')

    def get_complete_position(self, context):
        if not self.executable_racer:
            return -1

        if context['event'] != 'Manual' \
                and not re.search(r'(\.|::)\w*$', context['input']):
            return -1

        results = self.get_results('prefix')
        if not results:
            return -1
        prefixline = results[0]
        return int(prefixline[7:].split(',')[0])

    def gather_candidates(self, context):
        typeMap = { 'Struct' : 's', 'Module' : 'M', 'Function' : 'f',
                    'Crate' : 'C', 'Let' : 'v', 'StructField' : 'm',
                    'Impl' : 'i', 'Enum' : 'e', 'EnumVariant' : 'E',
                    'Type' : 't', 'FnArg' : 'v', 'Trait' : 'T'
                    }

        candidates = []
        for line in [l[6:] for l in self.get_results('complete')
                     if l.startswith('MATCH')]:
            completions = line.split(',',5)
            kind = typeMap[completions[4]]
            completion = {'kind' : kind, 'word' : completions[0]}
            if kind == 'f': # function
                completion['abbr'] = \
                    completions[5].replace('pub ','').replace(
                        'fn ','').rstrip('{')
                if int(self.vim.eval('g:racer_insert_paren')):
                    completion['word'] += '('
                completion['info'] = completions[5]
            elif kind == 's' : # struct
                completion['abbr'] = \
                    completions[5].replace('pub ','').replace(
                        'struct ','').rstrip('{')
            candidates.append(completion)
        return candidates

    def get_results(self, command):
        temp = self.vim.eval('tempname()')
        with open(temp, 'w') as f:
            for l in self.vim.current.buffer:
                f.write(l + "\n")
        try:
            results = subprocess.check_output([
                    self.racer, command,
                    str(self.vim.eval('line(".")')),
                    str(self.vim.eval('col(".")-1')),
                    temp
                ]).decode(self.encoding).splitlines()
        except subprocess.CalledProcessError as err:
            return []
        finally:
            os.remove(temp)
        return results
