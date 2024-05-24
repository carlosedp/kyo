package kyo.llm

import kyo.*

case class Model(name: String, maxTokens: Int)

object Model:
    val gpt4            = Model("gpt-4", 8192)
    val gpt4_turbo      = Model("gpt-4-1106-preview", 128000)
    val gpt4_vision     = Model("gpt-4-vision-preview", 128000)
    val gpt4_32k        = Model("gpt-4-32k", 32768)
    val gpt35_turbo     = Model("gpt-3.5-turbo", 4097)
    val gpt35_turbo_16k = Model("gpt-3.5-turbo-16k", 16385)
end Model

case class Config(
    apiUrl: String,
    apiKey: Option[String],
    apiOrg: Option[String],
    model: Model,
    temperature: Double,
    maxTokens: Option[Int],
    seed: Option[Int],
    completionMeter: Meter,
    completionTimeout: Duration,
    embeddingMeter: Meter,
    embeddingTimeout: Duration
):
    def apiUrl(url: String): Config =
        copy(apiUrl = url)
    def apiKey(key: String): Config =
        copy(apiKey = Some(key))
    def model(model: Model): Config =
        copy(model = model)
    def temperature(temperature: Double): Config =
        copy(temperature = temperature.max(0).min(2))
    def maxTokens(maxTokens: Option[Int]): Config =
        copy(maxTokens = maxTokens)
    def seed(seed: Option[Int]): Config =
        copy(seed = seed)

    def completionMeter(meter: Meter): Config =
        copy(completionMeter = meter)
    def completionTimeout(timeout: Duration): Config =
        copy(completionTimeout = timeout)

    def embeddingMeter(meter: Meter): Config =
        copy(embeddingMeter = meter)
    def embeddingTimeout(timeout: Duration): Config =
        copy(embeddingTimeout = timeout)
end Config

object Config:
    val default =
        val apiKeyProp = "OPENAI_API_KEY"
        val apiOrgProp = "OPENAI_API_ORG"
        val apiKey     = sys.env.get(apiKeyProp).orElse(sys.props.get(apiKeyProp))
        val apiOrg     = sys.env.get(apiOrgProp)
        Config(
            "https://api.openai.com/v1",
            apiKey,
            apiOrg,
            Model.gpt4_turbo,
            0.7,
            None,
            None,
            Meters.initNoop,
            3.minutes,
            Meters.initNoop,
            3.minutes
        )
    end default
end Config

object Configs:

    private val local = Locals.init(Config.default)

    def get: Config < IOs =
        local.get

    def apiKey: String < IOs =
        get.map(_.apiKey.getOrElse(IOs.fail[String]("Can't locate the OpenAI API key")))

    def let[T, S](f: Config)(v: T < S): T < (IOs & S) =
        let(_ => f)(v)

    def let[T, S](f: Config => Config)(v: T < S): T < (IOs & S) =
        local.get.map { c =>
            local.let(f(c))(v)
        }
end Configs
