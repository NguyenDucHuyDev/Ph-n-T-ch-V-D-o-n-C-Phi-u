import express from "express"
import apiStockRouter from "./routers/apiStock.js" 

const app = express()
const port = 5000

app.use(express.json())
app.use("/api/stock", apiStockRouter);

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})