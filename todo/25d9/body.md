An unclosed local struct reports expected identifier once per following statement. is_item_resume stops the member list at item keywords but not at statement starts, so var and return are read as field names.

    func main() i32 {
        struct Point {
            x: i32
        var a: i32 = 1
        var b: i32 = 2
        return a + b
    }

Four errors for one fault.
