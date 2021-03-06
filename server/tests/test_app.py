from src.app import app, image_path

client = app.test_client()


def test_usecase() -> None:
    res = client.post("/upload_image")
    assert res.status_code == 400
    assert res.get_json() == {'error':
                              'uploadFile is required.'}

# TODO
# filename not exists test
# success


def test_image_path() -> None:
    assert image_path("a", "b") == "/server/static/task/a/b.jpg"
