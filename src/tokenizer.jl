using Automa

const unicode = re"α|β|γ|δ|ϵ|ε|ϕ|φ|ζ|η|θ|ϑ|ι|κ|λ|μ|ν|ξ|ρ|σ|ς|υ|χ|ψ|ω"
const first_letter   = re"[A-Za-z_]" | unicode
const general_letter = first_letter | re"[0-9]" 

const prefloat = re"[-+]?([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)"
const integer = re"[0-9]+"
const float   = prefloat | ((prefloat | re"[-+]?[0-9]+") * re"[eE][-+]?[0-9]+")

const qasm_tokens = [
        :identifier   => first_letter * rep(general_letter),
        :irrational   => re"π|pi|τ|tau|ℯ|ℇ|euler",
        :comma        => re",",
        :colon        => re":",
        :semicolon    => re";",
        :question     => re"\?",
        :equal        => re"=",
        :lparen       => re"\(",
        :rparen       => re"\)",
        :lbracket     => re"\[",
        :rbracket     => re"]",
        :lbrace       => re"{",
        :rbrace       => re"}",
        :annot        => re"@[*]",
        :at           => re"@",
        :version      => re"OPENQASM",
        :input        => re"input",
        :output       => re"output",
        :pragma       => re"#pragma",
        :qubit        => re"qubit",
        :hw_qubit     => re"$[0-9]+",
        :gate_def     => re"gate",
        :function_def => re"def",
        :if_block     => re"if",
        :else_block   => re"else",
        :switch_block => re"switch",
        :while_block  => re"while",
        :in_token     => re"in",
        :for_block    => re"for",
        :return_token => re"return",
        :control_mod  => re"ctrl",
        :negctrl_mod  => re"negctrl",
        :inverse_mod  => re"inv",
        :power_mod    => re"pow",
        :measure      => re"measure",
        :arrow_token  => re"->",
        :reset_token  => re"reset",
        :delay_token  => re"delay",
        :barrier_token => re"barrier",
        :void         => re"void",
        :const_token  => re"const",
        :assignment   => re"=|-=|\+=|\*=|/=|^=|&=|\|=|<<=|>>=",
        :operator     => re"-|\+|\++|\*|\*\*|/|%|&|&&|\||\|\||^|!|!=|~|>|<|<<|>>|>=|<=|=>|==",
        :boolean      => re"true|false",
        :bitstring    => re"\"([01] _?)* [01]\"",
        :all_token    => re"all",
        :break_token  => re"break",
        :mutable      => re"mutable",
        :readonly     => re"readonly",
        :builtin_gate => re"gphase|U",
        :alias        => re"let",
        :box          => re"box",
        :end_token    => re"end",
        :dim_token    => re"#dim[ ]?=[ ]?[0-7]",
        :im_token     => re"im",
        :case         => re"case",
        :default      => re"default",
        :keyword      => re"creg|qreg",
        :oct          => re"0o[0-7]+",
        :bin          => re"(0b|0B)[0-1]+",
        :hex          => re"0x[0-9A-Fa-f]+",
        :dot          => re"\.",
        :integer_token => integer,
        :float_token   => float,
        :include_token   => re"include",
        :continue_token  => re"continue",
        :octal_integer   => re"0o([0-7]_?)* [0-7]",
        :hex_integer     => re"(0x|0X) ([0-9a-fA-F] _?)* [0-9a-fA-F]",
        :hardware_qubit  => re"$ [0-9]+",
        :line_comment    => re"//",
        :block_comment   => re"/\*.*?\*/",
        :char            => '\'' * (re"[ -&(-~]" | ('\\' * re"[ -~]")) * '\'',
        :string_token    => '"' * rep(re"[ !#-~]" | re"\\\\\"") * '"' | '\'' * rep(re"[ -&(-~]" | ('\\' * re"[ -~]")) * '\'',
        :newline         => re"\r?\n",
        :spaces          => re"[\t ]+",
        :classical_type    => re"bool|uint|int|float|angle|complex|array|bit|stretch|duration",
        :durationof_token  => re"durationof", # this MUST be lower than classical_type to preempt duration
        :port_token        => re"port",
        :frame_token       => re"frame",
        :waveform_token    => re"waveform",
        #:pulse_function    => re"newframe|shift_phase|get_phase|set_phase|get_frequency|set_frequency|shift_frequency|sum|mix|scale|phase_shift|play|set_frequency",
        :duration_literal  => (float | integer) * re"dt|ns|us|ms|s|\xce\xbc\x73", # transcode'd μs
        :cal_token         => re"cal",
        :defcal_token      => re"defcal",
        :extern_token      => re"extern",
        :defcalgrammar_token => re"defcalgrammar",
]

@eval @enum Token error $(first.(qasm_tokens)...)
make_tokenizer((error,
    [Token(i) => j for (i,j) in enumerate(last.(qasm_tokens))]
)) |> eval

function closing_token(t::Token)
    t == lbrace && return rbrace
    t == lbracket && return rbracket
    t == lparen && return rparen
end
