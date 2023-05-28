import wa from "whatsapp-web.js";

const { Client, LocalAuth } = wa

export const _createClient = () =>
  new Client({
    authStrategy: new LocalAuth(),
    puppeteer: {
      headless: true,
      executablePath: "chromium"
    }
  });

export const _initializeClient = (client) => () => client.initialize();

export const _getChats = client => () => client.getChats();

export const _getChatById = client => id => () => client.getChatById(id);

export const _getState = maybe => client => () => client.getState().then(x => !!x ? maybe.just(x) : maybe.nothing);
