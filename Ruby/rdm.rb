# File: rdm.rb
# Date: 02/10/2010
# Author: Collin J. Doering <rekahsoft@gmail.com>
# Description: Random source file to experiment while learning ruby

class Point
  attr_accessor :x, :y

  def initialize(x=0,y=0)
    @x, @y = x, y
  end

  def self.add(p1,p2)
    Point.new(p1.x + p2.x, p1.y + p2.y)
  end
 end
