    type image = {
      width : int;
      height : int;
      palette : ColorTable.t;
      pixels : (int * int * int) array;
    }