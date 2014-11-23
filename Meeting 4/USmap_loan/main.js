google.load('visualization', '1', {'packages': ['geomap']});
   //google.setOnLoadCallback(drawDummy);
   dummy = [
        ['City', 'Popularity'],
        ['Boston', 1000]
      ]

    function drawDummy() {
      var google_dt = google.visualization.arrayToDataTable(dummy);

      var options = {};
      options['region'] = 'US';
      options['colors'] = [0xFF8747, 0xFFB581, 0xc06000]; //orange colors
      options['dataMode'] = 'markers';

      var container = document.getElementById('map_canvas');
      var geomap = new google.visualization.GeoMap(container);
      geomap.draw(google_dt, options);
    };

    // take the pre-loaded master_array variable
    // and transform it into Geomap variable, master_geodata
    function loadData() {
      master_geodata = [];
      for (var i = 0; i < master_array.length; i++) {
        master_geodata.push(google.visualization.arrayToDataTable
          (master_array[i]));
      }

      options = {};
      options['region'] = 'US';
      options['colors'] = [0xFF8747, 0xFFB581, 0xc06000]; //orange colors
      options['dataMode'] = 'markers';

      console.log("Done loading");
    };

    // draws the pre-loaded geodata onto the geomap
    year_counter = 0;
    function stepFunction() {
      geomap.draw( master_geodata[year_counter], options);
      year_counter = year_counter + 1;
      if (year_counter == master_geodata.length) {
        year_counter = 0;
      }
    };

    // Todo: Just create 11 different divs
    function drawMapArray() {
      for (var i = 0; i < master_geodata.length; i++) {
        var container = document.createElement('div');
        $(container).appendTo( $("#map_canvas") );

        var p = document.createElement('p');
        var txt = document.createTextNode((i+first_year).toString());
        p.appendChild(txt);
        container.appendChild(p);

        var canvas = document.createElement('div');
        container.appendChild(canvas);

        var geomap = new google.visualization.GeoMap(canvas);
        geomap.draw(master_geodata[i], options);
      }
      console.log("Done drawing");
    }