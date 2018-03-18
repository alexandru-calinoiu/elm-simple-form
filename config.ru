require 'rack/app'
require 'rack/cors'

use Rack::Cors do
    allow do
        origins '*'
        resource '*', {
            headers: :any,
            methods: [:post]
        }
    end
end

class Server < Rack::App
    post '/api/contact' do
        p payload
    end
end

run Server