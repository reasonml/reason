import re, os
import sublime, sublime_plugin
from subprocess import Popen, PIPE
from collections import namedtuple

# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

ERROR_RE = re.compile(
    r'^File "(?P<file_name>.*)", line (?P<line>\d+), characters (?P<col>\d+)-\d+:$\r?\n'
    r'^Error: (?P<message>.+)$',
    re.MULTILINE
)

def is_interface(file_name):
    if file_name is None:
        # Should we assume it's an interface or implementation?
        raise Exception('Need file name')
    elif file_name.endswith('.rei'):
        return 'true'
    elif file_name.endswith('.re'):
        return 'false'
    else:
        raise Exception('Not a reason file: {}'.format(file_name))

def reason_command_line(file_name):
    settings = sublime.load_settings('Preferences.sublime-settings')
    reasonfmt = settings.get('reasonfmt_bin', 'reasonfmt')

    return [
        reasonfmt,
        '-use-stdin', 'true',
        '-parse', 're',
        '-is-interface-pp', is_interface(file_name),
        file_name
    ]

class ReasonFormatCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        settings = sublime.load_settings('Preferences.sublime-settings')
        max_width = str(settings.get('reason_max_width', 80))
        file_name = self.view.file_name();

        try:
            proc = Popen(reason_command_line(file_name) + [
                '-print', 're',
                '-print-width', max_width,
            ], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        except FileNotFoundError:
            error = 'Can\'t find `%s` ($PATH=%s).\n\n' \
                'You can set reasonfmt binary in your settings (Cmd+,) ' \
                'by adding "reasonfmt_bin": "/a/b/c".' \
                % (reasonfmt, os.environ['PATH'])
            sublime.error_message(error)
            return

        region = sublime.Region(0, self.view.size())
        source = self.view.substr(region)
        (stdout, stderr) = proc.communicate(bytes(source, 'utf8'))

        if len(stdout) > 0:
            formatted = stdout.decode('utf8')
            if source != formatted:
                self.view.replace(edit, region, formatted)
            else:
                sublime.status_message('Already formatted')
        else:
            match = ERROR_RE.match(stderr.decode('utf8'))
            if match:
                line = int(match.group('line'))
                col = int(match.group('col'))
                start = self.view.text_point(line - 1, col)
                self.view.sel().clear()
                self.view.sel().add(sublime.Region(start))
                self.view.show(start)

            self.view.run_command('sublimelinter_lint')


from SublimeLinter.lint import Linter

class Reason(Linter):
    syntax = 'reason'

    # Hack to make SublimeLinter happy, we build real command line in `cmd`
    executable = 'cat'

    regex = ERROR_RE
    multiline = True
    line_col_base = (1, 0)

    def cmd(self):
        return reason_command_line(self.view.file_name()) + ['-print', 'none']

