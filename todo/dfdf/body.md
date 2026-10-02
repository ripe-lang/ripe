func f() i64 { get() } where get returns type slot = i64 traps with SIGILL. The MIR has unreachable where the return should be.
