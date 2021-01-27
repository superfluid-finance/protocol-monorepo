module.exports = `<html>
<head>
    <style type="text/css">
body {
  font-family: "Open Sans", sans-serif;
  line-height: 1.25;
}

table {
  border: 1px solid #ccc;
  border-collapse: collapse;
  margin: 0;
  padding: 0;
  width: 100%;
  table-layout: fixed;
}

table caption {
  font-size: 1.5em;
  margin: .5em 0 .75em;
}

table tr {
  background-color: #f8f8f8;
  border: 1px solid #ddd;
  padding: .35em;
}

table th,
table td {
  padding: .625em;
  text-align: center;
  max-width: 100px;
 overflow: hidden;
 text-overflow: ellipsis;
}

table th {
  font-size: .85em;
  letter-spacing: .1em;
  text-transform: uppercase;
}

@media screen and (max-width: 600px) {
  table {
    border: 0;
  }

  table caption {
    font-size: 1.3em;
  }

  table thead {
    border: none;
    clip: rect(0 0 0 0);
    height: 1px;
    margin: -1px;
    overflow: hidden;
    padding: 0;
    position: absolute;
    width: 1px;
  }

  table tr {
    border-bottom: 3px solid #ddd;
    display: block;
    margin-bottom: .625em;
  }

  table td {
    border-bottom: 1px solid #ddd;
    display: block;
    font-size: .8em;
    text-align: right;
  }

  table td::before {
    /*
    * aria-label has no advantage, it won't be read inside a table
    content: attr(aria-label);
    */
    content: attr(data-label);
    float: left;
    font-weight: bold;
    text-transform: uppercase;
  }

  table td:last-child {
    border-bottom: 0;
  }
}

    </style>

</head>
<body>
  <h1>{{TITLE}}</h1>

  <h2>Stats</h2>
  <div class="table-wrapper">
      <table class="fl-table">
          <thead>
          <tr>
              {{HEADERS-STATS}}
          </tr>
          </thead>
          <tbody>
              {{BODY-STATS}}
          <tbody>
      </table>
  </div>

  <h2>Records</h2>
  <div class="table-wrapper">
      <table class="fl-table">
          <thead>
          <tr>
              {{HEADERS-TX}}
          </tr>
          </thead>
          <tbody>
              {{BODY-TX}}
          <tbody>
      </table>
  </div>
</body>
</html>`;
