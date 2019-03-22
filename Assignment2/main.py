from flask import Flask, request, redirect, url_for, render_template, send_from_directory
from PIL import Image
import os

server = Flask(__name__)

UPLOAD_FOLDER = 'uploads/'
server.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER


@server.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST':
        try:
            file = request.files['file']
        except KeyError:
            return redirect(url_for('index'))

        if not file.filename:
            return redirect(url_for('index'))

        file.save(os.path.join(server.config['UPLOAD_FOLDER'], file.filename))

        return redirect(url_for('uploaded_file', filename=file.filename))
    else:
        return server.send_static_file('index.html')


@server.route('/show/<filename>')
def uploaded_file(filename):
    path = os.path.join(server.config['UPLOAD_FOLDER'], filename)

    # image to process
    img = Image.open(path)

    filename = '/uploads/' + filename
    return render_template('result.html', filename=filename)


@server.route('/uploads/<filename>')
def send_file(filename):
    return send_from_directory(UPLOAD_FOLDER, filename)


if __name__ == '__main__':
    server.run(host='0.0.0.0', port=5001)
