import express from "express";
import stockJson from "../utils/stockJson.js"

const router = express.Router()

router.get("/AAPL" , (req,res,next) =>{
    res.json(stockJson.AAPL)
})

router.get("/GOOG" , (req,res,next) =>{
    res.json(stockJson.GOOG)
})

router.get("/TSLA" , (req,res,next) =>{
    res.json(stockJson.TSLA)
})

router.get("/AMZN" , (req,res,next) =>{
    res.json(stockJson.AMZN)
})

router.get("/META" , (req,res,next) =>{
    res.json(stockJson.META)
})

router.get("/KO" , (req,res,next) =>{
    res.json(stockJson.KO)
})

router.get("/PEP" , (req,res,next) =>{
    res.json(stockJson.PEP)
})


export default router;
