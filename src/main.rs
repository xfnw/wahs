extern crate warc;
extern crate rouille;
use rouille::Response;

fn main() {
    rouille::start_server("localhost:8000", move |request| {
        println!("{:?}", request.url());
        {
            if request.url() == "/hello" {
                return Response::text("hello world");
            }
        }
        return Response::html(
            "<h1>error: 404 not found</h1>\n\
                the page you requested is not part of this warc file.",
        );
    });
}
