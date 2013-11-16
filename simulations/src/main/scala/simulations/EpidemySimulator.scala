package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
  }

  import SimConfig._

  var persons: List[Person] = List() // to complete: construct list of persons
  
  for (index <- 1 to 3) {
    val p = Person(index)
    p.infect
    persons = persons ::: List(p)
  }
  
  for (index <- 4 to 300) {
    val p = Person(index)
    persons = persons ::: List(p)
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    val random = new java.util.Random()
    afterDelay(random.nextInt(5) + 1) {
      move
    }
    
    def infect(): Unit = {
      if (!infected && !immune) {
        infected = true
        afterDelay(6) { sick = true }
        afterDelay(14) { 
          if (random.nextInt(100) < 25) dead = true
        }
        afterDelay(16) {
          if (!dead) {
            immune = true
            sick = false
          }
        }
        afterDelay(18) {
          if (!dead) {
            infected = false
            sick = false
            immune = false 
          }
        }
      }
    }
    
    def move: Unit = {
      if (dead) {
        return
      }
      val direction = random.nextInt(4) % 4
      if (direction == 0) {
        var maybeCol = (col - 1) % SimConfig.roomColumns
        if (maybeCol < 0) maybeCol = SimConfig.roomColumns - 1
        if (persons.count(p => p.col == maybeCol && p.row == row && (p.sick || p.dead)) == 0) {
          col = maybeCol
          if (persons.count(p => p.infected || p.sick || p.dead) > 0 && random.nextInt(100) < 40) infect
        }
      }
      else if (direction == 1) {
        var maybeRow = (row + 1) % SimConfig.roomRows // right
        if (maybeRow >= SimConfig.roomRows) maybeRow = 0
        if (persons.count(p => p.row == maybeRow && p.col == col && (p.sick || p.dead)) == 0) {
          row = maybeRow
          if (persons.count(p => p.infected || p.sick || p.dead) > 0 && random.nextInt(100) < 40) infect
        }
      } 
      else if (direction == 2) {
        var maybeCol = (col + 1) % SimConfig.roomColumns // down
        if (maybeCol >= SimConfig.roomColumns) maybeCol = 0
        if (persons.count(p => p.col == maybeCol && p.row == row && (p.sick || p.dead)) == 0) {
          col = maybeCol
          if (persons.count(p => p.infected || p.sick || p.dead) > 0 && random.nextInt(100) < 40) infect
        }
      }
      else {
        var maybeRow = (row - 1) % SimConfig.roomRows // left
        if (maybeRow < 0) maybeRow = SimConfig.roomRows - 1
        if (persons.count(p => p.row == maybeRow && p.col == col && (p.sick || p.dead)) == 0) {
          row = maybeRow
          if (persons.count(p => p.infected || p.sick || p.dead) > 0 && random.nextInt(100) < 40) infect
        }
      }
      
      if (!dead) {
    	afterDelay(random.nextInt(5) + 1) {
	      move
	    }
      }
    }
  }
  
  object Person {
    def apply(id: Int) = new Person(id)
  }
}
