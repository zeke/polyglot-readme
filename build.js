var fs = require("fs")
var path = require("path")
var handlebars = require("handlebars")
var template = handlebars.compile(fs.readFileSync("./template.hbs", "utf-8"))
var finder = require("findit")("./detect")
var langs = []

finder.on("file", function (file, stat) {
  if (!file.match(".txt")) return
  langs.push({
    name: file.split("/")[1],
    snippet: fs.readFileSync(file, "utf-8")
  })

})

finder.on("end", function () {
  console.log(template({langs: langs}))
  fs.writeFileSync("./README.md", template({langs: langs}))
})
