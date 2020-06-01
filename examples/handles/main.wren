foreign class Counter {
    construct new() {}

    foreign ping
    foreign static time
}

class Main {
    static main() {
        var counter = Counter.new()
        for (i in 0..1000) {
            System.print("%(counter.ping) at time %(Counter.time)")
        }
    }
}