import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable.{ArrayBuffer, Stack}

object Main {

  def findNeighborhoods(cells: Array[Array[Int]], height: Int, width: Int, n: Int): Array[Array[Int]] = {

    //Iterate through the cells array searching for positive cells
    //When a positive cell is found push the coordinates onto a stack
    //Calculate adjacent cells and push those coordinates onto the stack
    //Continue to do so until the distance threshold is reached
    //If another positive cell is located at one of the adjacent cells, recalculate distance

    //Enumeration for cells in neighborhoods array
    // 0 = non-neighborhood cell
    // 1 = neighborhood cell

    //Declare neighborhoods array
    val neighborhoods = Array.ofDim[Int](height, width)

    //Declare variable to track distance traveled from positive cell
    var distance_traveled = 0

    //Declare stack to track adjacent coordinates
    var st = Stack[(Int, Int)]()

    //Initialize direction vectors for adjacent cells
    val d_x = Array(0, 1, 0, -1)
    val d_y = Array(-1, 0, 1, 0)

    //Declare x and y variables for use tracking coordinates
    var x: Int = 0
    var y: Int = 0

    //Declare arraybuffer for tracking positive cell (home) locations
    val homes = new ArrayBuffer[(Int, Int)]()

    //Iterate through cells array until a positive cell is located
    for(i <- 0 until height) {
      for (j <- 0 until width) {

        //Check if cell is positive and not already part of a neighborhood
        if (cells(i)(j) == 1 && neighborhoods(i)(j) == 0) {

          var coord = (i, j)

          //Push coordinates onto stack
          st.push(coord)

          //Reset distance traveled variable
          distance_traveled = 0

          //Add location to homes arraybuffer
          homes += coord

          //Iterate through stack until empty
          while (!st.isEmpty) {
            breakable {

              //store value of top element in stack and pop it off
              var curr = st.pop()

              //first element in tuple is y value
              y = curr._1
              //second element in tuple is x value
              x = curr._2

              //Check if coordinates are outside array bounds, if so break loop
              if (y < 0 || x < 0 || y >= height || x >= width) break()

              //Check if cell has already been visited, if so break loop
              if (neighborhoods(y)(x) != 0) break()

              //Initialize variables for smallest distance
              var smallest_distance = 100

              //Calculate distance traveled from nearest home
              for (k <- 0 until homes.length) {

                //Calculate manhattan distance from current cell (y,x) to home (homes(k)._1, homes(k)._2)
                var temp_distance = (y - homes(k)._1).abs + (x - homes(k)._2).abs

                //If calculated distance is less than the current smallest distance, smallest distance becomes temp distance
                if (temp_distance < smallest_distance) smallest_distance = temp_distance
              }

              //Set distance_travelled to smallest calculated distance
              distance_traveled = smallest_distance

              //Check if within distance threshold (distance_traveled < n)
              if (distance_traveled <= n) {

                //Mark current cell as within neighborhood
                neighborhoods(y)(x) = 1

                //Check if cell is positive, if so cell contains home
                if (cells(y)(x) > 0) {

                  //Initialize flag to signify home presence
                  var home_present = false

                  //Check if cell does not exist in homes arraybuffer
                  for (k <- 0 until homes.length) {

                    //If cell exists in array set presence flag to true
                    if (homes(k)._1 == y && homes(k)._2 == x) home_present = true

                  }

                  //Add location to homes array if not already present
                  if (!home_present) homes.addOne(y, x)
                }

                //Push adjacent cells onto stack
                for (k <- 0 until 4) {

                  //Ensure all cells on adjacent vectors and within distance threshold are pushed onto stack
                  var l = 1
                  for (l <- 1 to n) {

                    var adj = (y + l * d_y(k), x + l * d_x(k))
                    st.push(adj)

                    //For example given y = 2, x = 2 or (2,2) with a distance threshold of 2:
                    //(2,0), (4,2), (2,4), (0,2) and
                    //(2,1), (3,2), (2,3), (1,2) will be pushed onto the stack

                    //This fixes an issue where a cell within the distance threshold will be "hidden"
                    //due to its adjacent cells already being captured by a previous home's neighborhood
                  }
                }
              }
            }
          }

          //Clear homes arraybuffer
          homes.clear()
        }
      }
    }

    return neighborhoods
  }

  def calcNeighborhoodArea(cells: Array[Array[Int]], height: Int, width: Int, n: Int): Int = {

    //Initialize neighborhood area variable to 0
    var neighborhood_area = 0

    //Initialize arraybuffer to track coordinates of homes
    var homes = new ArrayBuffer[(Int, Int)]()

    var i = 0
    var j = 0

    //Find cells that contain a positive integer
    for(i <- 0 until height) {
      for(j <- 0 until width) {

        //Add location to homes arraybuffer
        if(cells(i)(j) == 1) homes.addOne(i, j)

      }
    }

    //Initialize distance, area, overhang, and overlap variables
    var d = 0
    var d_x = 0
    var d_y = 0
    var area = 0
    var overlap: Float = 0
    var overhang = 0
    var overhang_overlap = 0

    //Loop through homes array
    for(i <- 0 until homes.length) {

      //Reset overhang and overhang_overlap
      overhang = 0
      overhang_overlap = 0

      //Create x and y variables to hold current coordinates
      var y = homes(i)._1
      var x = homes(i)._2

      //Calculate area for current home using equation n^2 + (n+1)^2
      area += n * n + (n + 1) * (n + 1)

      //Account for lower bound x edge case (x < n)
      if (x < n) {
        //Calculate overhang area as (n - x)^2
        overhang += (n - x) * (n - x)
      }

      //Account for upper bound x edge case (x > width - 1 - n)
      if (x > (width - 1 - n)) {
        //Calculate overhang area as (n - (width - 1- x))^2
        overhang += (n - (width - 1 - x)) * (n - (width - 1 - x))
      }

      //Account for lower bound y edge case (y < n)
      if (y < n) {
        //Calculate overhang area as (n - y)^2
        overhang += (n - y) * (n - y)
      }

      //Account for upper bound y edge case (y > height - 1 - n)
      if (y > (height - 1 - n)) {
        //Calculate overhang area as (n - (height - 1- y))^2
        overhang += (n - (height - 1 - y)) * (n - (height - 1 - y))
      }

      //Account for Upper Left corner case (y < n && x < n)
      if (y < n && x < n) {

        //Calculate overlap from overhang calculations in edge cases

        //Overlap limit is (n - y) + (n - x) - n - 1
        //Simplified down to limit = n - y - x - 1
        var overlap_limit = n - y - x - 1

        //Sum until overlap limit is reached
        for(j <- 1 to overlap_limit) overhang_overlap += j
      }

      //Account for Upper Right corner case (y < n && x > width - 1 - n)
      if (y < n && x > (width - 1 - n)) {

        //Calculate overlap from overhang calculations in edge cases

        //Overlap limit is (n - y) + (n - (width - 1 - x)) - n - 1
        //Simplified down to limit = n - y - width + x
        var overlap_limit = n - y - width + x

        //Add up overlap until limit is reached
        for (j <- 1 to overlap_limit) overhang_overlap += j
      }

      //Account for Bottom Left corner case (y > height - 1 - n && x < n)
      if (y > (height - 1 - n) && x < n) {

        //Calculate overlap from overhang calculations in edge cases

        //Overlap limit is (n - (height - 1 - y)) + (n - x) - n - 1
        //Simplified down to limit = n - height + y - x
        var overlap_limit = n - height + y - x

        //Add up  overlap until limit is reached
        for (j <- 1 to overlap_limit) overhang_overlap += j
      }

      //Account for Bottom Right corner case (y > height - 1 - n && x > width - 1 - n)
      if (y > (height - 1 - n) && x > (width - 1 - n)) {

        //Calculate overlap from overhang calculations in edge cases

        //Overlap limit is (n - (height - 1 - y)) + (n - (width - 1 - x)) - n - 1
        //Simplified down to limit = n - height + y - width + x + 1
        var overlap_limit = n - height + y - width + x + 1

        //Add up  overlap until limit is reached
        for (j <- 1 to overlap_limit) overhang_overlap += j
      }

      area = area - overhang + overhang_overlap

      //Loop through homes array again, starting from i
      for(j <- i until homes.length){

        breakable {

          //Reset overlap
          overlap = 0

          //Calculate distance between (x,y) and homes(j)
          d_y = (y - homes(j)._1).abs
          d_x = (x - homes(j)._2).abs
          d = d_y + d_x

          if(d == 0) break()

          //Check if distance between (x, y) and homes(j) is less than 2*n + 1
          //If this is the case then there is no intersection between neighborhoods
          if(d < 2 * n + 1) {

            //Calculate overlap
            //num_diags is max number of diagonal lines overlapping between points
            // 2*n + 1 is max distance between points to cause overlap, d is manhattan distance between points
            val num_diags: Float = 2 * n + 1 - d

            //Calculate overlap with the equation
            // overlap = num_diags * (n - |(d_x - d_y)/2|) + num_diags/2 + (num_diags % 2)
            // where length of each diagonal line is n - |(d_x - d_y)/2|
            // and extra length not captured by n - |(d_x - d_y)/2| because of lines alternating size
            // is captured by num_diags/2 + (num_diags % 2)

            val diag_length = n.toFloat - ((d_x.toFloat - d_y.toFloat) / 2).abs
            val diag_extra: Float = (num_diags / 2) + (num_diags % 2)

            overlap = num_diags * diag_length + diag_extra
          }

          //Subtract the overlap area calculated from the total area calculated
          area = area - overlap.toInt

        }

      }
    }

    return area

  }
  def main(args: Array[String]): Unit = {

    var height: Int = 0
    var width: Int = 0
    var n: Int = 0
    var gridType: String = "random"

    //Prompt user for grid setup information
    println("Would you like to setup a random or directed grid?")
    gridType = scala.io.StdIn.readLine()

    //Prompt user for grid height
    println("Grid height (1 to 100):")
    height = scala.io.StdIn.readInt()

    //Prompt user for grid width
    println("Grid width (1 to 100):")
    width = scala.io.StdIn.readInt()

    //Prompt user for distance threshold
    println("Neighborhood Distance Threshold (1 to 10):")
    n = scala.io.StdIn.readInt()

    //create 2 dimensional array using height and width values
    val cells = Array.ofDim[Int](height, width)

    //Random setup
    if(gridType == "random") {

      //Generate random number seed
      val rand = new util.Random()

      //Loop through cells array
      var i = 0
      var j = 0
      for(i <- 0 until height) {
        for(j <- 0 until width) {

          //Generate random number
          var rand_int = rand.nextInt()

          if(rand_int % 17 == 0)
            cells(i)(j) = 1
          else
            cells(i)(j) = -1

        }
      }
    }

    //Manual setup
    else if(gridType == "directed") {

      var row = 0
      var col = 0

      //Prompt user to enter cell locations in row, col format
      println("Enter locations for positive integers in row, column format. Enter -1 to finish.")

      breakable {

        //loop until invalid row or col value is entered
        while (row >= 0 && col >= 0 && row < height && col < width) {

          println("Row: ")
          row = scala.io.StdIn.readInt()
          if (row < 0 || row >= height)
            break

          println("Col: ")
          col = scala.io.StdIn.readInt()
          if (col < 0 || col >= width)
            break

          //Set location in grid to 1
          cells(row)(col) = 1

        }
      }
    }
    //Incorrect gridType entered
    else {
      println("Incorrect input received. Application exiting.")
      return
    }

    //Print initial array
    var i = 0
    var j = 0
    for(i <- 0 until height) {
      for(j <- 0 until width) {
        if(cells(i)(j) == 1) {
          if(j == width - 1)
            println(" +")
          else
            print(" +")
        }
        else {
          if(j == width - 1)
            println(" .")
          else
            print(" .")
        }
      }
    }

    //Print breaker
    println("********************************************************")

    //Create array to track neighborhoods
    var neighborhoods = Array.ofDim[Int](height, width)

    //Initialize array to 0s
    for(i <- 0 until height) {
      for( j <- 0 until width) {
        neighborhoods(i)(j) = 0
      }
    }

    //Find neighborhoods
    neighborhoods = findNeighborhoods(cells, height, width, n)

    //Variable to track number of cells in neighborhoods
    var n_cells = 0

    //Print neighborhoods array
    for(i <- 0 until height) {
      for(j <- 0 until width) {
        if(neighborhoods(i)(j) == 1){
          if(j == width-1) println(" o")
          else print(" o")
          n_cells += 1
        }
        else {
          if (j == width - 1) println("  ")
          else print("  ")
        }
      }
    }

    //Print number of cells in neighborhoods found via findNeighborhoods function
    println("There are " + n_cells + " cells within neighborhoods.")

    //Print number of cells in neighborhoods found via calcNeighborhoodArea function
    println("calcNeighborhoodArea output: " + calcNeighborhoodArea(cells, height, width, n))
  }
}