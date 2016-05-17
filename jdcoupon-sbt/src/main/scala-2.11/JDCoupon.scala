import com.alibaba.fastjson.{JSON, JSONObject}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


/**
  * Created by wunan on 2016/5/6.
  */


object JDCoupon {

    val itemRegexp ="""<li data-sku="(\d+).*?title="(.+?)" href="(.*?)".*?data-price="(\d+.\d+).*?data-stock-val="(\d)"""".r
    val totalRegexp ="""(?<=LogParm.result_count=")(\d+)""".r


    val catRegexp ="""(?<=cat: \[)([0-9,]+)""".r
    val skuidRegexp ="""(?<=skuid: )(\d+)""".r
    val venderRegexp ="""(?<=venderId:)(\d+)""".r

    val area: String = "10_698_45817"   // haerbin 

    val showState = true

    def showListMap(l: Seq[Map[String, String]])(keySet: Seq[String]): Unit = {
        l.foreach { x =>
            println(keySet.map(x.get(_).getOrElse("")).mkString("\t"))
        }
    }

    def getState(url: String): String = showState match {
        case true => getStockInfo(url)
        case _ => "忽略"
    }

    def getStockInfo(url: String): String = {
        val callback = "getStockCallback"
        //println(url)
        val productContent = Source.fromURL(url, "GBK").getLines().take(100).mkString(" ")
        val infos = Map("cat" -> catRegexp, "skuid" -> skuidRegexp, "venderId" -> venderRegexp).map(x => (x._1, x._2.findFirstIn(productContent).getOrElse("0"))).toMap

        Thread.sleep(500)

        val params = "http://c0.3.cn/stock?skuId=%s&cat=%s&area=%s_0&buyNum=1&callback=%s&ch=1&extraParam={%%22originid%%22:%%221%%22}".format(infos.get("skuid").get, infos.get("cat").get, area, callback)
        //println(params)
        val json = JSON.parseObject(Source.fromURL(params, "GBK").getLines().mkString("").drop(callback.size + 1).dropRight(1))

        Option(json.getJSONObject("stock").getString("StockStateName")).getOrElse("没找到")

    }

    /**
      * @param url match url like http://search.jd.com/Search?coupon_batch=31680954&coupon_id=5116305736
      */
    def showCouponProductList(url: String): Unit = {
        val couponProductListUrl = url match {
            case u: String if u.startsWith("search.jd.com/Search") || u.startsWith("http://search.jd.com/Search") =>
                "http://search.jd.com/s_new.php?" + u.substring(u.indexOf('?') + 1) + "&rt=1&vt=2&psort=3&"
            case _ => "http://search.jd.com/s_new.php?" + url + "&rt=1&vt=2&psort=3&"
        }

        var page = 1
        var s = 1
        var maxNum = 0
        val allProductList = ArrayBuffer[Map[String, String]]()

        do {

            val reqUpperUrl = page match {
                case n: Int if n % 2 == 1 => couponProductListUrl + s"page=${page}&s=${s}&click=0"
                case _ => couponProductListUrl + s"page=${page}&s=${s}&scrolling=y"
            }
            //println(s"reqUrl: ${reqUpperUrl}")

            val upperPageContent = Source.fromURL(reqUpperUrl, "UTF-8").getLines().mkString("")

            for (itemRegexp(id, desc, url, money, state) <- itemRegexp.findAllIn(upperPageContent)) {
                allProductList += Map("id" -> id, "desc" -> desc.replace(" ", ","), "url" -> s"http:${url}", "money" -> money)
            }

            page += 1
            s += 30

            maxNum = totalRegexp.findFirstIn(upperPageContent).getOrElse("0").toInt

        } while (s < maxNum)

        showListMap(allProductList)(List("id", "desc", "url", "money"))

    }

    /**
      * @param url match url like http://coudan.jd.com/coudan.html?188778961
      */
    def showCoudanProductList(url: String): Unit = {

        val key = url.substring(url.indexOf('?') + 1)
        val callback = "jQuery4593390"
        val allProductList = ArrayBuffer[Map[String, String]]()
        var page = 1
        var totalPage = 1
        do {
            val reqUrl = s"http://coudan.jd.com/coudan/listProducts.jsonp?callback=${callback}&wr.currentPage=${page}&wr.orderValue=&wr.lowPrice=&wr.highPrice=&wr.targetId=${key}&wr.cids=&wr.defaultMoney=1000&wr.locationId=10-698-45819-0&wr.pageSize=20&wr.spareSize=30&wr.thirdCid="

            val pageContent = Source.fromURL(reqUrl, "UTF-8").getLines().mkString("").drop(callback.size + 1).dropRight(1)
            val json = JSON.parseObject(pageContent)
            totalPage = json.getIntValue("totalPage")

            json.getJSONArray("list").toArray().foreach { item =>
                val o = item.asInstanceOf[JSONObject]
                val url = "http://item.jd.com/%s.html".format(o.getString("id"))
                allProductList += Map("id" -> o.getString("id"), "desc" -> o.getString("name").replace(" ", ","), "url" -> url)
            }
            page += 1

        } while (page <= totalPage)

        showListMap(allProductList)(List("id", "desc", "url"))


    }

    def showList(url: String): Unit = url match {
        case url: String if url.indexOf("search.jd.com/Search") > -1 => showCouponProductList(url)
        case url: String if url.indexOf("coudan.jd.com/") > -1 => showCoudanProductList(url)
    }

    def begin(args: Array[String]): Unit = {
        try {
            if (args.size < 2) {
                showHelp()
                return
            }
            args(0) match {
                case "analyze" => showList(args(1))
                case "share" => showShareProductList(args.drop(1))
                case "stock" => addStockInfo(args(1))
                case _ => showHelp(); return
            }
        } catch {
            case e: Exception => e.printStackTrace()
        }
    }

    def showShareProductList(files: Array[String]): Unit = {

        files.map(Source.fromFile(_, "GBK").getLines().filter(_.size > 5).map(_.split("\\s"))
            .map(x => (x(0), x.toList)).toMap).reduce((x, y) => x.filterKeys(y.contains(_)))
            .map(x => (x._1, x._2 :+ getState(s"http://item.jd.com/${x._1}.html"))).values
            .foreach(x => println(x.mkString("\t")))
    }

    def addStockInfo(file: String): Unit = {
        Source.fromFile(file).getLines().map(_.split("\\s").toList)
            .map(x => x :+ getState(s"http://item.jd.com/${x(0)}.html"))
            .foreach(x => println(x.mkString("\t")))
    }


    def main(args: Array[String]) {
        begin(args)

    }

    def showHelp(): Unit = {
        println(
            """
              |usage:
              |    analyze url
              |    share file1 file2
              |    stock file1
            """.stripMargin)
    }


}
