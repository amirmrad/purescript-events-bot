import qrcode from "qrcode-terminal";

export const _showQr = (code) => () => qrcode.generate(code, { small: true });
