import java.util.Date

object JSONSerialization extends App {

  /* imagine social network user-case. we have
    - Users, posts, feeds
    - we are backend devs, we want to serialize these to JSON so that we can pass them to our front end or inbetween our multiple services (if we want to work in microservice architecture)
   */

  // want to serialize these 3 things
  // because our code base is huge, doesn't make sense to implement in case classes themselves; want it to be extensible
  // want other people to be able to write to our code base
  // therefore, we use type class to solve this
  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, posts: List[Post])

  /* Steps
    1 - create intermediate data types which can be string-ified as json from string, int, and primitive data types
      - want JSON serializable data types for Int, String, List, Date
      - basically create things that look exactly like JSON
    2 - create type classe(s) for conversion from our normal scala case classes to our intermediate data types
    3 - serialize intermediate data types to JSON
   */

  /**** 1 -  intermediate data type; trait starts hierarchy *****/
  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = "\"" + value + "\""
  }

  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    override def stringify: String = values.map(_.stringify).mkString("[", ",", "]")
  }

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    /*
      This will be intermediate representation for the kind of object that looks like a javascript object

      {
      name: "John"
      age: 22
      friends: [..]
      latestPost: {
        content: "Scala Rocks"
        date: ...
        }
      }
     */
    override def stringify: String = values.map {
      case (key, value) => "\"" + key + "\":" + value.stringify
    }
      .mkString("{", ",", "}")
  }

  val data = JSONObject(Map(
    "user" -> JSONString("Daniel"),
    "posts" -> JSONArray(List(
      JSONString("Scala Rocks"),
      JSONNumber(453)
    ))
  ))

  println(data.stringify)

  /******** 2 *************/
  /* type class; need to create a few things here
    1 - type class
    2 - type class instances (implicit)
    3 - need method to use our type class instances
        - pimp lib to use type class instances
   */

  // 2.1 type class
  trait JSONConverter[T] {
    def convert(value: T): JSONValue // users or posts or feeds and converts to JSON value
  }

  //2.2 type class instances
  //2.2.1 - existing data types
  implicit object StringConverter extends JSONConverter[String] {
    override def convert(value: String): JSONValue = JSONString(value)
  }

  implicit object NumberConverter extends JSONConverter[Int] {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }
  //2.2.2 - custom data types
  implicit object UserConverter extends JSONConverter[User] {
    override def convert(value: User): JSONValue = JSONObject(Map(
      "name" -> JSONString(value.name),
      "age" -> JSONNumber(value.age),
      "age" -> JSONString(value.email)
    ))
  }

  implicit object PostConverter extends JSONConverter[Post] {
    override def convert(value: Post): JSONValue = JSONObject(Map(
      "content" -> JSONString(value.content),
      "created_at" -> JSONString(value.createdAt.toString)
    ))
  }

//  implicit object FeedConverter extends JSONConverter[Feed] {
//    override def convert(value: Feed): JSONValue = JSONObject(Map(
//
//      // clunky; look at FeedConverterImproved
//      "user" -> UserConverter.convert(value.user),
//      "posts" -> JSONArray(value.posts.map(PostConverter.convert))
//    ))
//  }

  // 2.3 conversion

  // both Ops and Enrichment are naming conventions in practice
  // you call x.toJSON
  // compiler sees that type of X does not have toJSON method
  // compiler looks for all types that can wrap 'x' and turn it into something with a toJSON method
  // compiler will then find a type class instance with the same type class as x and use that json converter
  implicit class JSONOps[T](value: T) {
    def toJSON(implicit converter: JSONConverter[T]): JSONValue =
      converter.convert(value)
  }

  /* equivalent imps

    implicit class JSONOps[T: JSONConverter](value: T) {
    def toJSON: JSONValue = {
      val converter: JSONConverter[T] = implicitly[JSONConverter[T]]
      converter.convert(value)
    }
  }


   */

  /**** 3 - call stringify on result******/

  val now = new Date(System.currentTimeMillis())
  val john = User("John", 34, "john@rockthejvm.com")
  val feed = Feed(john, List(
    Post("hello", now),
    Post("look at this cute puppy", now)
  ))
  // converts our case class to intermediate JSON representation and then call stringify on that result
  println(feed.toJSON.stringify)


  implicit object FeedConverterImproved extends JSONConverter[Feed] {
    override def convert(value: Feed): JSONValue = JSONObject(Map(
      "user" -> feed.user.toJSON,
      "posts" -> JSONArray(value.posts.map(_.toJSON))
      // clunky; look at FeedConverterImproved
      //"user" -> UserConverter.convert(value.user),
      //"posts" -> JSONArray(value.posts.map(PostConverter.convert))
    ))
  }


}

