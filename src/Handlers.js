export const _onMessage = c => h => () => c.on('message', h)
export const _onMessageCreate = c => h => () => c.on('message_create', h)
export const _onQr = c => h => () => c.on('qr', h)
export const _onChangeState = c => h => () => c.on('change_state', h)
export const _onceReady = c => h => () => c.once('ready', h)
