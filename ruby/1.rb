class Hello
    def hello
        puts("1")
    end

end

class World < Hello

    def self.say_hello
        puts("2")
    end

end


