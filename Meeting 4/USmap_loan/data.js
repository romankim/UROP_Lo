function handleFileSelect(evt) {
    var file = evt.target.files[0]
    csv_data = []

    // use Papaparse to parse csv data with javascript
    // http://papaparse.com/
    // http://www.joyofdata.de/blog/parsing-local-csv-file-with-javascript-papa-parse/
    Papa.parse(file, {
        header: true,
        dynamicTyping: true,
        complete: function(results) {
            csv_data = results;

// load the csv data
// 'master_array' is a 3D array s.t. master[year]= [[Boston, 100], [NY, 150],...]
// 'data' is the 1-year snapshot
            first_year = csv_data.data[0].time
            last_year = csv_data.data[(csv_data.data.length-2)].time
            master_array = [];
            for (var i=first_year; i <= last_year; i++ ) {
                master_array.push([['City', 'Loan']])
            }

            for (var i = 0; i < csv_data.data.length -1; i++) {
                var yr = csv_data.data[i].time
                var idx = yr-first_year

                var tuple = []
                tuple.push(csv_data.data[i].city)
                tuple.push(csv_data.data[i].loan)

                master_array[idx].push(tuple)
            }

            loadData();
            drawMapArray();
        }

    });


}

$(document).ready(function() {
    $("#csv-file").change(handleFileSelect);
});