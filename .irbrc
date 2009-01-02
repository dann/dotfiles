require 'rubygems'
require 'irb/completion'
require 'pp'

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY]=1000

module Kernel
  def r(arg)
    puts `refe #{arg}`
  end
  private :r
end

class Module
  def r(meth = nil)
    if meth
      if instance_methods(false).include? meth.to_s
        puts `refe #{self}##{meth}`
      else
        super
      end
    else
      puts `refe #{self}`
    end
  end
end

