#!/usr/bin/env elixir

defmodule While do
    def while(condition, acc, body) do
        should_continue = true
        if should_continue and condition.() do
            acc = case body.(acc) do
                {:continue, acc1} ->
                    while(condition, acc1, body)
                {:break, acc1} ->
                    should_continue = false
                    acc1
                acc1 ->
                    while(condition, acc1, body)
            end
        end
        acc
    end
end

defmodule CppHeaderParser do
    @moduledoc false

    alias __MODULE__

    @kBLOCK_TYPE     0
    @kBLOCK_NAME     1
    @kPROCESS_FLAG   2
    @kPUBLIC_SECTION 3
    @kCLASS_DECL     4

    # states:
    @kSCAN           0 # outside of a comment or preprocessor directive
    @kCOMMENT        1 # inside a multi-line comment
    @kDIRECTIVE      2 # inside a multi-line preprocessor directive
    @kDOCSTRING      3 # inside a multi-line docstring
    @kDIRECTIVE_IF_0 4 # inside a '#if 0' directive

    defstruct [
        namespaces: MapSet.new(),
        generate_umat_decls: false,
        generate_gpumat_decls: false,
        hname: :nil,
        block_stack: :nil,
        wrap_mode: false,
        lineno: 0,
        state: @kSCAN,
        depth_if_0: 0,
        docstring: "",
        block_head: ""
    ]

    def new(generate_umat_decls, generate_gpumat_decls) when is_boolean(generate_umat_decls) and is_boolean(generate_gpumat_decls) do
        %CppHeaderParser{generate_umat_decls: generate_umat_decls, generate_gpumat_decls: generate_gpumat_decls}
    end
    def new, do: new(true, true)

    def wrap_ok({:ok, val}), do: val

    @doc """
    adds the dot-separated container class/namespace names to the bare function/class name, e.g. when we have

    namespace cv {
    class A {
    public:
        f(int);
    };
    }

    the function will convert "A" to "cv.A" and "f" to "cv.A.f".
    """
    def get_dotted_name(self=%CppHeaderParser{}, name) do
        if Enum.count(self.block_stack) == 0 do
            name
        else
            if String.starts_with?(name, "cv.") do
                name
            else
                qualified_name = (String.match?(name, Regex.compile!(".")) or String.match?(name, Regex.compile!("::")))
                n = for b <- self.block_stack, reduce: "" do
                    n ->
                        {block_type, block_name} = {Enum.at(b, @kBLOCK_TYPE), Enum.at(b, @kBLOCK_NAME)}
                        if not Enum.member?(["file", "enum"], block_type) do
                            if not Enum.member?(["enum struct", "enum class"], block_type) or block_name != name do
                                if not Enum.member?(["struct", "class", "namespace", "enum struct", "enum class"], block_type) do
                                    IO.puts("Error at #{self.lineno}: there are non-valid entries in the current block stack #{self.block_stack}")
                                    1 != 1
                                else
                                    if String.length(block_name) > 0 and (block_type == "namespace" or not qualified_name) do
                                        n = n <> block_name <> "."
                                    end
                                end
                            end
                        end
                        n
                end
                n = n <> String.replace(name, "::", ".")
                if String.ends_with?(n, ".Algorithm") do
                    n = "cv.Algorithm"
                end
                n
            end
        end
    end

    def get_macro_arg(self=%CppHeaderParser{}, arg_str, npos) do
        s_arg_str = String.slice(arg_str, npos, String.length(arg_str) - npos)
        npos2 = case :binary.match(s_arg_str, "(") do
            :nomatch ->
                IO.puts("Error: no arguments for the macro at #{self.hname}:#{self.lineno}")
                1 != 1
            {npos2, _} -> npos2 + npos
        end
        balance = 1
        npos3 = npos2
        {npos3, balance} = While.while(fn -> true end, {npos3, balance}, fn {npos3, balance} ->
            {t, npos3} = find_next_token(arg_str, ["(", ")"], npos3+1)
            if npos3 < 0 do
                IO.puts("Error: no matching ')' in the macro call at #{self.hname}:#{self.lineno}")
                1 != 1
            end
            if t == "(" do
                balance = balance + 1
            end
            if t == ")" do
                balance = balance - 1
            end

            if balance == 0 do
                {:break, {self, balance}}
            else
                {self, balance}
            end
        end)

        {String.slice(arg_str, npos2+1, npos3-npos2+1) |> String.trim(), npos3}
    end

    @doc """
    Parses class/struct declaration start in the form:
        {class|struct} [CV_EXPORTS] <class_name> [: public <base_class1> [, ...]]
    Returns class_name1, <list of base_classes>
    """
    def parse_class_decl(self=%CppHeaderParser{}, decl_str) do
        l = decl_str
        modlist = []
        if String.match?(l, "CV_EXPORTS_W_MAP") do
            l = String.replace(l, "CV_EXPORTS_W_MAP", "", global: true)
            modlist = [modlist | "/Map"]
        end
        if String.match?(l, "CV_EXPORTS_W_SIMPLE") do
            l = String.replace(l, "CV_EXPORTS_W_SIMPLE", "", global: true)
            modlist = [modlist | "/Simple"]
        end

        npos = case :binary.match(l, "CV_EXPORTS_AS") do
            {pos, _} -> pos
            _ -> -1
        end

        if npos < 0 do
            npos = case :binary.match(l, "CV_WRAP_AS") do
                {npos2, _} -> npos2
                :nomatch -> -1
            end
        end

        if npos > 0 do
            {macro_arg, npos3} = get_macro_arg(self, l, npos)
            modlist = [modlist | "=" <> macro_arg]
            l = String.slice(l, 0, npos) <> String.slice(l, npos3 + 1, String.length(l) - npos3 + 1)
        end

        l = batch_replace(l, [
            {"CV_EXPORTS_W", ""},
            {"CV_EXPORTS", ""},
            {"public virtual ", " "},
            {"public ", " "},
            {"::", "."}])
            |> String.trim()
        ll = String.split(l, Regex.compile!("\\s+|\\s*[,:]\\s*"))
        ll = Enum.filter(ll, fn le -> String.length(le) > 0 end)
        classname = Enum.at(ll, 1)
        [_,_|bases] = ll
        {classname, bases, modlist}
    end

    def parse_enum(self=%CppHeaderParser{}, decl_str) do
        l = decl_str

        ll = String.split(l, ",", global: true)
        if List.last(ll) |> String.trim() == "" do
            ll = ll |> Enum.reverse() |> tl() |> Enum.reverse()
        end
        prev_val = ""
        prev_val_delta = -1
        decl = []
        for pair <- ll, reduce: {decl, prev_val, prev_val_delta} do
            {decl, prev_val, prev_val_delta} ->
                pv = String.split(pair, "=")
                val = ""
                if Enum.count(pv) == 1 do
                    prev_val_delta = prev_val_delta + 1
                    if prev_val != "" do
                        val = prev_val <> "+"
                    end
                    val = val <> Integer.to_string(prev_val_delta)
                else
                    prev_val_delta = 0
                    val = Enum.at(pv, 1) |> String.trim()
                    prev_val = val
                end
                decl = [decl | ["const " <> get_dotted_name(self, Enum.at(pv, 0) |> String.trim()), val, [], [], nil, ""]]
        end
    end

    @doc """
    parses the statement (ending with ';' or '}') or a block head (ending with '{')

    The function calls parse_class_decl or parse_func_decl when necessary. It returns
    <block_type>, <block_name>, <parse_flag>, <declaration>
    where the first 3 values only make sense for blocks (i.e. code blocks, namespaces, classes, enums and such)
    """
    def parse_stmt(self=%CppHeaderParser{}, stmt, end_token, mat \\ "Mat", docstring \\ "") do
        stack_top = List.last(self.block_stack)
        context = Enum.at(stack_top, @kBLOCK_TYPE)

        if String.starts_with?(stmt, "inline namespace") do
            # emulate anonymous namespace
            {"namespace", "", true, nil}
        else
            stmt_type = ""
            if end_token == "{" do
                stmt_type = "block"
            end

            if context == "block" do
                IO.puts("Error at #{self.lineno}: should not call parse_stmt inside blocks")
                1 != 1
            end

            if context == "class" or context == "struct" do
                self = While.while(fn -> true end, self, fn self ->
                    case :binary.match(stmt, ":") do
                        :nomatch ->
                            {:break, self}
                        {colon_pos, _} ->
                            w = String.trim(String.slice(stmt, 0, colon_pos))
                            if Enum.member?(["public", "protected", "private"], w) do
                                if w == "public" or (not self.wrap_mode and w == "protected") do
                                    stack_top = List.update_at(stack_top, @kPUBLIC_SECTION, fn _ -> true end)
                                else
                                    stack_top = List.update_at(stack_top, @kPUBLIC_SECTION, fn _ -> false end)
                                end
                                stmt =
                                    stmt
                                    |> String.slice(colon_pos + 1, String.length(stmt) - colon_pos - 1)
                                    |> String.trim()
                                block_len = Enum.count(self.block_stack)
                                self = %{self | block_stack: List.update_at(self.block_stack, block_len - 1, fn _ -> stack_top end)}
                            end
                            {:break, self}
                    end
                end)
            end

            if not Enum.at(stack_top, @kPUBLIC_SECTION) or String.starts_with?(stmt, "template") do
                {stmt_type, "", false, nil}
            else
                if end_token == "{" do
                    if not self.wrap_mode and String.starts_with?(stmt, "typedef struct") do
                        stmt_type = "struct"
                        offset = String.length("typedef ")
                        case parse_class_decl(self, String.slice(stmt, offset, String.length(stmt) - offset)) do
                            :error ->
                                IO.puts("Error at #{self.hname}:#{self.lineno}")
                                1 != 1
                            {classname, bases, modlist} ->
                                if String.starts_with?(classname, "_Ipl") do
                                    classname = String.slice(classname, 1, String.length(classname) - 1)
                                end
                                decl = [stmt_type <> " " <> get_dotted_name(self, classname), "", modlist, [], nil, docstring]
                                if bases != nil do
                                    decl_1 = ": " <> (Enum.map(bases, fn b ->
                                        get_dotted_name(self, b)
                                        |> String.replace(".", "::", global: true)
                                    end)
                                    |> Enum.join(", "))
                                    decl = List.update_at(decl, 1, fn _ -> decl_1 end)
                                end
                                {stmt_type, classname, true, decl}
                        end
                    end

                    # todo
                    if String.starts_with?(stmt, "class") or String.starts_with?(stmt, "struct") do
                        # stmt_type = stmt.split()[0]
                        # if stmt.strip() != stmt_type:
                        #     try:
                        #         classname, bases, modlist = self.parse_class_decl(stmt)
                        #     except:
                        #         print("Error at %s:%d" % (self.hname, self.lineno))
                        #         exit(1)
                        #     decl = []
                        #     if ("CV_EXPORTS_W" in stmt) or ("CV_EXPORTS_AS" in stmt) or (not self.wrap_mode):# and ("CV_EXPORTS" in stmt)):
                        #         decl = [stmt_type + " " + self.get_dotted_name(classname), "", modlist, [], None, docstring]
                        #         if bases:
                        #             decl[1] = ": " + ", ".join([self.get_dotted_name(b).replace(".","::") for b in bases])
                        #     return stmt_type, classname, True, decl
                    end

                    if String.starts_with?(stmt, "enum") or String.starts_with?(stmt, "namespace") do
                        # NB: Drop inheritance syntax for enum
                        stmt = String.split(stmt, ":") |> Enum.at(0)
                        splits = String.split(stmt, " ")
                        if Enum.count(splits) >= 2 do
                            stmt_list_0 = Enum.join(splits |> Enum.reverse() |> tl() |> Enum.reverse(), " ")
                            stmt_list_1 = List.last(splits)
                            {stmt_list_0, stmt_list_1, true, nil}
                        else
                            {splits, "<unnamed>", true, nil}
                        end
                    end

                    if String.starts_with?(stmt, "extern") and String.match?(stmt, "\"C\"") do
                        {"namespace", "", true, nil}
                    end
                end

                if end_token == "}" and context.startswith("enum") do
                    # todo: parse_enum
                    decl = parse_enum(self, stmt)
                    name = Enum.at(stack_top, @kBLOCK_NAME)
                    {context, name, false, decl}
                end

                if end_token == ";" and String.starts_with?(stmt, "typedef") do
                    # TODO: handle typedef's more intelligently
                    {stmt_type, "", false, nil}
                end

                # todo
                # paren_pos = stmt.find("(")
                # if paren_pos >= 0:
                #     # assume it's function or method declaration,
                #     # since we filtered off the other places where '(' can normally occur:
                #     #   - code blocks
                #     #   - function pointer typedef's
                #     decl = self.parse_func_decl(stmt, mat=mat, docstring=docstring)
                #     # we return parse_flag == False to prevent the parser to look inside function/method bodies
                #     # (except for tracking the nested blocks)
                #     return stmt_type, "", False, decl

                # if (context == "struct" or context == "class") and end_token == ";" and stmt:
                #     # looks like it's member declaration; append the members to the class declaration
                #     class_decl = stack_top[self.CLASS_DECL]
                #     if ("CV_PROP" in stmt): # or (class_decl and ("/Map" in class_decl[2])):
                #         var_modlist = []
                #         if "CV_PROP_RW" in stmt:
                #             var_modlist.append("/RW")
                #         stmt = self.batch_replace(stmt, [("CV_PROP_RW", ""), ("CV_PROP", "")]).strip()
                #         var_list = stmt.split(",")
                #         var_type, var_name1, modlist, argno = self.parse_arg(var_list[0], -1)
                #         var_list = [var_name1] + [i.strip() for i in var_list[1:]]

                #         for v in var_list:
                #             class_decl[3].append([var_type, v, "", var_modlist])
                #     return stmt_type, "", False, None

                # # something unknown
                # return stmt_type, "", False, None
            end
        end
    end

    @doc """
    Finds the next token from the 'tlist' in the input 's', starting from position 'p'.
    Returns the first occurred token and its position, or ("", len(s)) when no token is found
    """
    def find_next_token(s, tlist, p \\ 0) do
        token = nil
        tpos = String.length(s)
        sp = String.slice(s, p, tpos - p)
        for t <- tlist, reduce: {token, tpos} do
            {nil, tpos} ->
                case :binary.match(sp, t) do
                    {mpos, _len} ->
                        if mpos < tpos do
                            {t, mpos}
                        else
                            {token, tpos}
                        end
                    _ ->
                        {token, tpos}
                end
            acc -> acc
        end
    end

    def parse(self=%CppHeaderParser{}, hname, wmode \\ true) when is_binary(hname) do
        decls = []
        linelist =
            hname
            |> File.read()
            |> wrap_ok()
            |> String.split("\n", trim: true)


        self = %{self | hname: hname, block_stack: [["file", hname, true, true, :nil]], wrap_mode: wmode, lineno: 0}

        for l0 <- linelist, reduce: self do
            self ->
                l = String.trim(l0)
                # IO.puts("state: #{self.state}, lineno: #{self.lineno}, #{l}")

                # G-API specific aliases
                l = batch_replace(l, [
                    {"GAPI_EXPORTS", "CV_EXPORTS"},
                    {"GAPI_EXPORTS_W", "CV_EXPORTS_W"},
                    {"GAPI_EXPORTS_W_SIMPLE","CV_EXPORTS_W_SIMPLE"},
                    {"GAPI_WRAP", "CV_WRAP"},
                    {"GAPI_PROP", "CV_PROP"},
                    {"GAPI_PROP_RW", "CV_PROP_RW"},
                    {"defined(GAPI_STANDALONE)", "0"}
                ])

                self = case {self.state, String.first(l) == "#"} do
                    {@kSCAN, true} ->
                        %{self | state: @kDIRECTIVE}
                    _ -> self
                end

                self = case {self.state, String.last(l)} do
                    {@kDIRECTIVE, "\\"} -> self
                    {@kDIRECTIVE, _} ->
                        self = %{self | state: @kSCAN}
                        processed_line =
                            l
                            |> String.replace(Regex.compile!("//(.+)"), "", global: true)
                            |> String.trim()
                        case is_directive_if_0(processed_line) do
                            true -> %{self | state: @kDIRECTIVE_IF_0, depth_if_0: 1}
                            false -> self
                        end
                    _ ->
                        if self.state == @kDIRECTIVE_IF_0 do
                            case {l, self.depth_if_0} do
                                {"#if" <> _, depth_if_0} ->
                                    %{self | state: @kDIRECTIVE_IF_0, depth_if_0: depth_if_0 + 1}
                                {"#endif" <> _, 0} ->
                                    %{self | state: @kSCAN, depth_if_0: 0}
                                {"#endif" <> _, depth_if_0} ->
                                    %{self | state: @kSCAN, depth_if_0: depth_if_0 - 1}
                                _ -> self
                            end
                        else
                            if self.state == @kCOMMENT do
                                {self, l} = case :binary.match(l, "*/") do
                                    {pos, _len} ->
                                        processed_line = String.slice(l, pos + 2, String.length(l) - pos - 2)
                                        {%{self | state: @kSCAN}, processed_line}
                                    :nomatch ->
                                        {self, l}
                                end
                            end

                            if self.state == @kDOCSTRING do
                                {self, l} = case :binary.match(l, "*/") do
                                    {pos, _len} ->
                                        processed_line = String.slice(l, pos + 2, String.length(l) - pos - 2)
                                        {%{self | state: @kSCAN, docstring: self.docstring <> String.slice(l, 0, pos) <> "\n"}, processed_line}
                                    :nomatch ->
                                        {%{self | docstring: self.docstring <> l0}, l}
                                end
                            end

                            if String.starts_with?(l, "CV__") or String.starts_with?(l, "__CV_") do
                                %{self | state: @kSCAN}
                            else
                                if self.state != @kSCAN do
                                    IO.warn("Error at #{self.lineno}, invalid state = #{self.state}")
                                    1 != 1
                                else
                                    self = While.while(fn -> true end, self, fn self ->
                                        {token, pos} = case String.match?(l, Regex.compile!("=\\s*\\{\\s*\\}")) do
                                            {_pos, _len} ->
                                                {";", String.length(l)}
                                            false ->
                                                find_next_token(l, [";", "\"", "{", "}", "//", "/*"])
                                        end

                                        condition = String.length(self.docstring) <= 0 or String.last(self.docstring) != ")" or not String.starts_with?(self.docstring, "CV_ENUM_FLAGS(")

                                        ret = case {token, condition} do
                                            {nil, true} ->
                                                {:break, %{self | docstring: String.trim(self.docstring <> " " <> l)}}
                                            _ ->
                                                if condition == false do
                                                    l = ""
                                                    token = ";"
                                                end

                                                if token != nil do
                                                    self = %{self | docstring: String.trim(self.docstring <> " " <> l)}
                                                end

                                                if token == "//" do
                                                    self = %{self | docstring: self.docstring <> " " <> String.slice(l, 0, pos)}
                                                    l = ""
                                                    {:continue, self}
                                                else
                                                    if token == "/*" do
                                                        self = %{self | docstring: self.docstring <> " " <> String.slice(l, 0, pos)}
                                                        tmp_l = String.slice(l, pos + 2, String.length(l) - pos - 2)
                                                        condition = (String.length(l) > pos + 2) and String.at(l, pos + 2) == "*"
                                                        case {:binary.match(tmp_l, "*/"),condition} do
                                                            {:nomatch, true} ->
                                                                self = %{self | docstring: String.slice(l, pos + 3, String.length(l) - pos - 3) <> "\n", state: @kDOCSTRING}
                                                                {:break, self}
                                                            {:nomatch, false} ->
                                                                {:break, %{self | state: @kCOMMENT}}
                                                            {{end_pos, _}, true} ->
                                                                {:continue, %{self | docstring: String.slice(l, pos + 3, end_pos)}}
                                                            {{end_pos, _}, false} ->
                                                                {:continue, self}
                                                        end
                                                    else
                                                        if token == "\"" do
                                                            pos2 = pos + 1
                                                            {self, pos2} = While.while(fn -> true end, {self, pos2}, fn {self, pos2} ->
                                                                {t2, pos2} = find_next_token(l, ["\\", "\""], pos2)
                                                                case t2 do
                                                                    nil ->
                                                                        IO.puts("Error at #{self.lineno}: no terminating '\"'")
                                                                        1 != 1
                                                                    "\"" ->
                                                                        {:break, {self, pos2}}
                                                                    _ ->
                                                                        pos2 = pos2 + 2
                                                                        {:continue, {self, pos2}}
                                                                end
                                                            end)

                                                            self = %{self | block_head: self.block_head <> "" <> String.slice(l, 0, pos2 + 1)}
                                                            {:continue, self}
                                                        else
                                                            stmt = String.trim(self.block_head <> "" <> String.slice(l, 0, pos))
                                                            stmt =
                                                                stmt
                                                                |> String.split()
                                                                |> Enum.join(" ")
                                                            if String.first(stmt) == "@" do
                                                                # Objective C ?
                                                                {:break, self}
                                                            else
                                                                stack_top = List.last(self.block_stack)

                                                                decls = nil
                                                                if Enum.at(stack_top, @kPROCESS_FLAG) do
                                                                    # even if stack_top[PUBLIC_SECTION] is False, we still try to process the statement,
                                                                    # since it can start with "public:"
                                                                    docstring = String.trim(self.docstring)
                                                                    {stmt_type, name, parse_flag, decl} = parse_stmt(self, stmt, token, docstring: docstring)
                                                                end
                                                                1 = 2
                                                                {:break, self}
                                                            end
                                                        end
                                                    end
                                                end
                                        end
                                        ret
                                    end)
                                end
                            end
                            self
                        end
                end

                %{self | lineno: self.lineno + 1}
        end
    end

    def batch_replace(line, [{pattern, replacement}|pairs]) when is_binary(line) and is_binary(pattern) and is_binary(replacement) do
        batch_replace(String.replace(line, pattern, replacement, global: true), pairs)
    end
    def batch_replace(line, []), do: line

    def is_directive_if_0("#if 0"), do: true
    def is_directive_if_0("#if defined(__OPENCV_BUILD)"), do: true
    def is_directive_if_0("#ifdef __OPENCV_BUILD"), do: true
    def is_directive_if_0("#if !defined(OPENCV_BINDING_PARSER)"), do: true
    def is_directive_if_0("#ifndef OPENCV_BINDING_PARSER"), do: true
    def is_directive_if_0(_), do: false

    def test do
        # c "hdr_parser.exs"
        # CppHeaderParser.test
        parser = CppHeaderParser.new
        # just for testing
        # [file|_] = System.argv()
        file = "/Users/cocoa/Git/evision/3rd_party/opencv/modules/core/include/opencv2/core/mat1.hpp"
        parsed = CppHeaderParser.parse(parser, file)
    end
end
